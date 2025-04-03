module Ryvm.Text.Selector where

import qualified Data.Text as Text
import System.FilePath (dropExtension)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Ryvm.Text.Clean (cleanText)

-- | Get the words/tokens from a selector, and normalize them.
--
-- Example:
-- >>> selectorWords "hello_world-foo bar"
-- ["hello","world","foo","bar"]
selectorWords :: FilePath -> [Text.Text]
selectorWords = pathWords . Text.toLower . cleanText . Text.pack

onlyParse :: [String]
onlyParse = [".gopher", ".txt", ".md", ".html", ".json", ".xml", ".csv", ".hs"]

-- | Get the words/tokens from a path.
--
-- Also enforces a maximum length.
--
-- Differs from `selectorWords` in that it's not focused on cleaning the text of any
-- ineappropriate characters.
--
-- Example:
-- >>> pathWords "hello_world-foo bar"
-- ["hello","world","foo","bar"]
-- >>> pathWords "hello/word/foo.gopher.txt"
-- ["hello","word","foo"]
pathWords :: Text.Text -> [Text.Text]
pathWords = Text.split (`elem` splitChars) . dropExtensionSpecial
  where
    splitChars = [' ', '_', '-', '.', '/']
    sortedExtensions = sortOn (Down . Text.length) (map Text.pack onlyParse)
    -- Drop the onlyParse extensions if it exists, otherwise just use dropExtension from FilePath
    dropExtensionSpecial :: Text.Text -> Text.Text
    dropExtensionSpecial fp = 
        let
          fpNoExt = dropExtension (Text.unpack fp)  -- Remove last extension if no specific match
        in
          case filter (`Text.isSuffixOf` fp) sortedExtensions of
            (ext:_) -> Text.dropEnd (Text.length ext) fp
            []      -> Text.pack fpNoExt