module Ryvm.Text.Selector where

import qualified Data.Text as T
import System.FilePath (dropExtension)
import Ryvm.Text.Clean (cleanText)
import qualified Data.ByteString as B
import Data.Text.Encoding (encodeUtf8)

-- | Get the words/tokens from a selector, and normalize them.
--
-- Example:
-- >>> selectorWords "hello_world-foo bar"
-- ["hello","world","foo","bar"]
selectorWords :: FilePath -> [T.Text]
selectorWords = pathWords . T.toLower . cleanText . T.pack

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
pathWords :: T.Text -> [T.Text]
pathWords = T.split (`elem` splitChars) . T.pack . dropExtension . T.unpack
  where
    splitChars = [' ', '_', '-', '.', '/']

-- | Get the byte length of a filename in UTF-8 encoding
filenameByteLength :: String -> Int
filenameByteLength filename = B.length . encodeUtf8 $ T.pack filename