-- TODO: Am I not stripping punctuation from keywords and tokens? That's a big deal. Also
-- need to ensure a standard function for doing so like `searchableText`.
-- TODO: improvements: exclude words like and, is, etc.
{- | Text file search and rank functionality.

Designed for use with the Bore SpacecookieClone, for the Gopher Protocol, in order to
implement search.

The terms "rank" and "score" are used interchangeably, but the `RankScore` is implemented
as a float where higher is better. Various factors which go into the score can be
manipuluated through the weights represented as constants in this module.

Possible improvements include using FrontMatter in ranking.

-}
module Ryvm.Search (getSearchResults, SearchResult(..)) where

import Ryvm.Text.Selector
import Ryvm.Text.Clean
import Ryvm.Verified
import Ryvm.WeightsTypes

import Data.List (foldl', sortOn)
import Data.List qualified as L
import Data.Text (Text, toLower)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>), makeRelative)

type AbsolutePath = FilePath

-- | Score the text for how well it matches keywords and return the score along with
-- snippets of where the matches were found.
rankDocument
    :: FilePath
    -- ^ The selector belonging to the document.
    -> [Text]
    -- ^ Keywords to search for.
    -> Text
    -- ^ Document to rank/score.
    -> (String, RankScore, [ContextSnippet])
    -- ^ The highlighted selector, the score, and the "contexts" where matches occur (in the body).
rankDocument selector keywords content =
  let contentWords = T.words . toLower $ cleanText content
      (highlightedSelector, totalScore, keywordMatches) = computeTotalScore selector keywords contentWords
      contexts = extractKeywordContexts keywordMatches contentWords
  in (highlightedSelector, totalScore, contexts)


-- | Compute total score based on proximity of different keyword matches and fuzzy matches
--
-- The total score is a combination of the following:
--
-- * Proximity score: The closer the keywords are to each other, the higher the score. A bonus for the same order, too.
-- * Frequency score: The more a keyword appears in the content, the higher the score.
-- * Fuzzy match score: The closer the fuzzy match is to the keyword, the higher the score.
-- * Exact match score: The more exact matches, the higher the score.
--
-- Example:
-- >>> tokens = T.words "Do you like tags? I like tags."
-- >>> keywords = ["I", "like", "tags"]
-- >>> fst $ computeTotalScore keywords tokens
-- 300.0
computeTotalScore
  :: FilePath
  -> [Text]
  -> [Text]
  -> (String, RankScore, [(Text, Int)])
  -- ^ The new selector with matches highlighted, the final score, and the keyword matches (with their indexes).
computeTotalScore selector keywords contentWords =
  let
    (keywordMatches, exactMatchScore) = computeMatchScore keywords contentWords
    (selectorHighlighted, selectorScore) = computeSelectorScore keywords selector
    finalScore = sum
      [ selectorScore
      , keywordOrderedProximity keywordMatches
      , computeFrequencyScore keywords contentWords
      --, computeFuzzyMatchScore keywords contentWords
      , exactMatchScore
      ]
  in
    (selectorHighlighted, finalScore, keywordMatches)

computeSelectorScore
  :: [Text]
  -> FilePath
  -> (String, RankScore)
computeSelectorScore keywords selector =
  let
    selectorWords' = selectorWords selector
    wordLikeness =
      [ (word, indx, if likeness == 100 then likeness * weightSelectorExact else likeness * weightSelectorFuzzy)
      | (word, indx, likeness) <- findKeywordMatches keywords selectorWords'
      ]
    likenessSum = sum $ map (\(_, _, likeness) -> likeness) wordLikeness

    -- Adjust positions dynamically as matches are added
    highlightedSelector =
      snd $ foldl
        (\(offset, acc) (word, indx, _) ->
            let
                -- Calculate start position, adjusted by the offset
                startPos = T.length (T.unwords $ take indx selectorWords') + offset + 1
                endPos = startPos + T.length word
                (prefix, rest) = splitAt startPos acc
                (toHighlight, suffix) = splitAt (endPos - startPos) rest
                -- Increment offset by the added length of the brackets
                addedBrackets = T.length "[" + T.length "]"
            in
                (offset + addedBrackets, prefix <> "[" <> toHighlight <> "]" <> suffix)
        )
        (0, selector)
        (sortOn (\(_, indx, _) -> indx) wordLikeness) -- Ensure matches are processed in order
  in
    (highlightedSelector, likenessSum)



-- FIXME: I think longer documents will benefit more or something? there's something like that i'm not thinking of here.
-- | Calculate proximity score for different keywords based on their positions. The closer
-- the keywords are to each other, the higher the score. A significant bonus is applied for
-- keywords appearing in the same order with small gaps.
--
-- Assumes @@keywordMatches@@ is supplied in the order keywords were given.
--
-- Example:
-- >>> keywordOrderedProximity [("hello", 0), ("world", 2), ("foo", 5)]
-- 28.57143
keywordOrderedProximity :: [(Text, Int)] -> RankScore
keywordOrderedProximity keywordMatches =
  let positions = map snd keywordMatches
      keywordPairs = zip positions (tail positions)
      distances = map (\(i, j) -> abs (i - j)) keywordPairs
      -- Adjusted calculation with threshold
      avgDistanceBonus = case distances of
        [] -> 0  -- No distances to calculate
        _  -> let (average :: Float) = fromIntegral (sum distances) / fromIntegral (length distances)
                  thresholdPenalty = if average > fromIntegral thresholdDistance then weightFarProximity else weightCloseProximity
              in thresholdPenalty * (1 / (average + 1))
  in avgDistanceBonus * weightOrderedProximity * fromIntegral (length positions)

-- Change this to do the fuzzy match and just do it off the batt and give bonus for 100% match.
computeMatchScore :: [Text] -> [Text] -> ([(Text, Int)], RankScore)
computeMatchScore keywords contentWords =
  let
    keywordMatches = findKeywordMatches keywords contentWords
    keywordMatchesBonus = [if likeness == 100 then likeness * weightExactMatch else likeness * weightFuzzyMatch | (_, _, likeness) <- keywordMatches]
    score = sum keywordMatchesBonus
    matchIndexes = [(t, indx) | (t, indx, _) <- keywordMatches]
  in
    (matchIndexes, score)

computeFrequencyScore :: [Text] -> [Text] -> RankScore
computeFrequencyScore keywords contentWords =
  let
    preliminaryScore = fromIntegral $ 5 * sum (map (keywordFrequency contentWords) keywords)
  in
    preliminaryScore * weightFrequency

-- | Count the number of times a keyword appears in the content
keywordFrequency :: [Text] -> Text -> Int
keywordFrequency contentWords keyword =
  length $ filter (== keyword) contentWords

-- | Extract and merge contexts for the found keyword matches, highlighting all keywords
extractKeywordContexts :: [(Text, Int)] -> [Text] -> [ContextSnippet]
extractKeywordContexts keywordMatches contentWords =
  let sortedMatches = L.sortOn snd keywordMatches  -- Sort matches by index
      mergedContexts = mergeCloseMatches sortedMatches
      contextSnippets = map (\(startIdx, endIdx) -> extractContextWithHighlights keywordMatches contentWords startIdx endIdx) mergedContexts
  in contextSnippets

-- | Merge keyword matches that are close enough to form a single context window
-- | Merge keyword matches that are close enough to form a single context window, but extend the context window before and after the match.
mergeCloseMatches :: [(Text, Int)] -> [(Int, Int)]
mergeCloseMatches [] = []
mergeCloseMatches ((_, idx):xs) = foldl' merge [(idx - contextWindowSize, idx + contextWindowSize)] xs
  where
    merge [] (_, nextIdx) = [(nextIdx - contextWindowSize, nextIdx + contextWindowSize)]  -- If acc is empty, start a new context
    merge acc@((start, end):rest) (_, nextIdx)
      | nextIdx - end <= contextWindowSize = (start, max (nextIdx + contextWindowSize) end) : rest -- Extend the current context
      | otherwise = (nextIdx - contextWindowSize, nextIdx + contextWindowSize) : acc  -- Start a new context

-- | Extract a context snippet and highlight all keywords within the context
-- | Extract a context snippet and highlight all keywords within the extended context window.
extractContextWithHighlights :: [(Text, Int)] -> [Text] -> Int -> Int -> ContextSnippet
extractContextWithHighlights keywordMatches contentWords startIdx endIdx =
  let contextWords = take (endIdx - startIdx + 1) $ drop startIdx contentWords
      highlightedWords = map (\(i, word) -> if any (\(_, kwIdx) -> kwIdx == i) keywordMatches then "[" <> word <> "]" else word) (zip [startIdx..] contextWords)
  in ContextSnippet startIdx endIdx (T.unwords highlightedWords)

-- | Check if the new snippet overlaps with existing ones based on indices
contextsOverlap :: ContextSnippet -> ContextSnippet -> Bool
contextsOverlap cs1 cs2 =
  csStart cs1 <= csEnd cs2 && csEnd cs1 >= csStart cs2

-- | Combine contexts, ensuring no overlaps
combineContexts :: [ContextSnippet] -> [ContextSnippet]
combineContexts = foldl' addContext []

addContext :: [ContextSnippet] -> ContextSnippet -> [ContextSnippet]
addContext acc cs =
  if any (contextsOverlap cs) acc
    then acc
    else acc ++ [cs]

-- ADD FUNCTION WHERE IF TEXT -> BOOL IS TRUE THEN APPLY FILTER FUNCTION TEXT -> TEXT FIXME
-- | Function to process multiple documents
searchDocuments
  :: Bool
  -> [String]
  -> Maybe (Text -> Text)
  -> [Text]
  -> AbsolutePath
  -> AbsolutePath
  -> IO [(FilePath, String, Bool, RankScore, [Text])]
  -- ^ The file path, the highlighted selector, whether it's a menu, the score, and the context snippets.
searchDocuments isRelative extWhitelist maybeFilterDocuments keywords sourceDirectoryAbsolutePath _ = do
  docPaths <- getWhitelistedFiles extWhitelist sourceDirectoryAbsolutePath
  docs <-
    mapM
      ( \fp -> do
          content <- loadFileContent fp
          let hackedContent = maybe content ($ content) maybeFilterDocuments
          pure (fp, False, hackedContent)
      )
      docPaths
  return $
    map
      ( \(fp, _, content) ->
          let
              relativePath' = if isRelative then makeRelative sourceDirectoryAbsolutePath fp else fp
              (highlightedSelector, score, contexts) = rankDocument relativePath' keywords content
              nonOverlappingContexts = combineContexts contexts
              contextTexts = map csText nonOverlappingContexts
           in (relativePath', highlightedSelector, False, score, contextTexts)
      )
      docs

-- Recursively search for files with whitelisted extensions in a directory
getWhitelistedFiles :: [String] -> FilePath -> IO [FilePath]
getWhitelistedFiles extWhitelist dir = do
  contents <- listDirectory dir
  paths <-
    mapM
      ( \name -> do
          let path = dir </> name
          isDir <- doesDirectoryExist path
          if isDir then getWhitelistedFiles extWhitelist path else return [path]
      )
      contents
  return $ filter (\f -> takeExtension f `elem` map ('.':) extWhitelist) (concat paths)

-- Load the content of a file
loadFileContent :: FilePath -> IO Text
loadFileContent filePath = T.toLower <$> TIO.readFile filePath

-- Main entrypoint. Should also have a prefix about search results when blank...
getSearchResults :: Bool -> [String] -> Maybe (Text -> Text) -> Text -> AbsolutePath -> AbsolutePath -> IO [SearchResult]
getSearchResults isRelative extWhitelist maybeFilterDocuments query sourceDirectoryAbsolutePath absoluteOutputPath = do
  documentResults <- searchDocuments isRelative extWhitelist maybeFilterDocuments (T.words . toLower $ query) sourceDirectoryAbsolutePath absoluteOutputPath
  let prunedResults = filter (\(_, _,  _, s, _) -> s >= scoreThreshold) documentResults
  pure $ searchResponse absoluteOutputPath query $ L.sortOn (\(_, _, _, s, _) -> negate s) prunedResults

data SearchResult = SearchResult
  { filePath :: FilePath
  , highlightedFilePath :: String
  , score :: RankScore
  , contexts :: Text
  } deriving (Show, Eq)

-- Function to generate a GopherResponse for search results
searchResponse
  :: AbsolutePath
  -> Text
  -> [(FilePath, String, Bool, RankScore, [Text])]
  -- ^ The selector (?) or filepath (?), the highlighted selector, whether it's a menu, the score, and the context snippets.
  -> [SearchResult]
searchResponse absoluteOutputPath _ files =
  map (makeSearchResult absoluteOutputPath) files

-- Build a search result for a file
makeSearchResult
  :: AbsolutePath
  -> (FilePath, String, Bool, RankScore, [Text])
  -- ^ The selector (?) or filepath (?), the highlighted selector, whether it's a menu, the score, and the context snippets.
  -> SearchResult
makeSearchResult _ (fp, highlightedSelector, _, score, contexts) =
  SearchResult
    { filePath = fp
    , highlightedFilePath = highlightedSelector
    , score = score
    , contexts = T.intercalate " ... " contexts
    }