{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Test suite for the search module.

The approach to testing the search functionality is to simplly test the ranking of the
search results, ensuring it ranks documents the way I'd desire and expect.

You can increase verbosity like this:

  stack test --test-arguments "--verbose"

-}

module Main (main) where

import Ryvm.Search
import Ryvm.Text.Selector

import System.Environment (getArgs)
import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
--import Test.DocTest (doctest)
import Test.QuickCheck
import qualified Data.Set as Set

-- | Check if what's supposed to be a filename is safe and sane.
--
-- Example:
-- >>> isSafeFilename "file.txt"
-- True
-- >>> isSafeFilename "../file.txt"
-- False
-- >>> isSafeFilename "file.txt/"
-- False
-- >>> isSafeFilename "file.txt/.."
-- False
-- >>> isSafeFilename "."
-- False
-- >>> isSafeFilename "/dir/file.txt"
-- False
-- >>> isSafeFilename "file"
-- True
isSafeFilename :: FilePath -> Bool
isSafeFilename name =
    isValid name              -- Checks if it contains only valid characters
        && not (null name)        -- Ensures it’s not empty
        && not (null $ takeFileName name)  -- Ensures it’s not just a directory
        && takeFileName name == name -- ensure it doesn't have a directory traversal
        && isRelative name     -- Ensures it's a relative path
        && name /= "." && name /= ".."  -- Disallows "." and ".."
        && filenameByteLength name <= 255  -- Ensures it's not too long

-- Increase the number of tests to run (e.g., 500)
--
-- High maxSuccess because testing search results can hide some weird edge cases.
--
-- This has a "chatty" field setting that might be useful in other cases.
testArgs :: Args
testArgs = stdArgs { maxSuccess = 5000 }

-- Main function that will soon hold doctests
main :: IO ()
main = do
  -- Check for a verbosity flag
  args <- getArgs
  let isVerbose = "--verbose" `elem` args

  putStrLn "Running QuickCheck properties..."
  quickCheckWith testArgs (prop_getSearchResultsRanking isVerbose)

-- | Maximum number of characters per keyword.
--
-- This limit was put in place because of testing keywords in filenames.
maxKeywordLength :: Int
maxKeywordLength = 10

-- | The maximum number of keywords to go in to the filename.
maxNumberKeywordsFilename :: Int
maxNumberKeywordsFilename = 4

defaultFileNames :: [FilePath]
defaultFileNames =
  [ "all_keywords.txt"
  -- ^ all keywords in order
  , "sprinkled_keywords.txt"
  -- ^ keywords "sprinkled" into a document
  , "no_keywords.txt"
  -- ^ no keywords in document
  ]

-- | The contents of the file that shouldn't match any keywords.
noKeywordsMessage :: Text
noKeywordsMessage = "This document does not contain any of the keywords we are searching for."

-- | Keywords that should not be used in the tests.
forbiddenKeywords :: [Text]
forbiddenKeywords = T.words noKeywordsMessage ++ concatMap selectorWords defaultFileNames

-- FIXME: put keywords into file name too but nothing in document, but still make that first.
-- Property to test if getSearchResults ranks documents as expected
prop_getSearchResultsRanking :: Bool -> Keywords -> Property
prop_getSearchResultsRanking isVerbose (Keywords keywords) = ioProperty $ do
  withSystemTempDirectory "test-search" $ \dir -> do
    -- Define file names and contents
    -- FIXME/TODO just add one keyword match. also how about a keyword in the file name?
    -- Fixme: ensure keyword safety with `isSafeFilename` somehow
    let
      fileNamedAfterKeywords = T.unpack $ T.intercalate "_" (take (min maxNumberKeywordsFilename $ length keywords) keywords) <> ".txt"
      (fileNames :: [FilePath]) = [fileNamedAfterKeywords] ++ defaultFileNames
    let expectedResults = init fileNames  -- in that order, too
    let contiguousKeywords = T.unwords keywords
    -- what about SPARSE keywords coming after this?
    let sprinkledOrderedKeywords = "This is some text with " <> T.intercalate " and " keywords <> " scattered."
    -- could even do one that's out-of-order! but above should test proximity

    -- Write files with specified contents
    let fileContents = [noKeywordsMessage, contiguousKeywords, sprinkledOrderedKeywords, noKeywordsMessage]
    forM_ (zip fileNames fileContents) $ \(fileName, content) -> do
      TIO.writeFile (dir </> fileName) content

    -- Call getSearchResults
    response <- getSearchResults Nothing (T.unwords keywords) dir dir

    -- Extract file paths in the returned results (ordered by rank)
    let rankedFiles = [takeFileName filepath | SearchResult {filePath=filepath} <- response]

    -- Useful information for debugging
    -- Can use: stack test --test-arguments "--verbose"
    if isVerbose
      then do
        putStrLn . show $ (zip fileNames fileContents)
        _ <- forM  (response) $ \f -> putStrLn $ show f
        putStrLn "\n"
        pure ()
      else
        pure ()
    
    return $ rankedFiles === expectedResults

-- could use isValid to ensure filenames are safe
instance Arbitrary Text where
  arbitrary = sized $ \n -> do
    let charGen = elements ['a'..'z']
    let forbiddenSet = Set.fromList forbiddenKeywords
    suchThat
      (do
        -- Limit length to the minimum of maxKeywordLength and the generated size
        len <- choose (2, min maxKeywordLength (2 + n))
        chars <- vectorOf len charGen
        return $ T.pack chars
      )
      (\txt -> txt `Set.notMember` forbiddenSet)

newtype Keywords = Keywords [Text]
  deriving (Show)

instance Arbitrary Keywords where
  arbitrary = sized $ \n -> do
    numKeywords <- chooseInt (2, min 10 (2 + n))  -- Generate between 2 and 10 keywords
    keywords <- vectorOf numKeywords arbitrary
    let combinedKeywords = T.unpack $ T.intercalate "_" keywords
    if isSafeFilename combinedKeywords
      then return $ Keywords keywords
      else arbitrary  -- Retry if the generated keywords don't form a valid filename