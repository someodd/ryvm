module Ryvm.Text.Tsv (searchResultsToTsv) where

import Ryvm.Search
import qualified Data.Text as T

escapeTabs :: T.Text -> T.Text
escapeTabs = T.replace "\t" "\\t"

searchResultToTsv :: SearchResult -> T.Text
searchResultToTsv result =
    (T.pack result.filePath) <> "\t" <> (T.pack result.highlightedFilePath) <> "\t" <> T.pack (show result.score) <> "\t" <> escapeTabs result.contexts

searchResultsToTsv :: [SearchResult] -> T.Text
searchResultsToTsv results =
    T.unlines $ map searchResultToTsv results