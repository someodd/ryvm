module Ryvm.CLI where

import Ryvm.Text.Tsv (searchResultsToTsv)
import Ryvm.Search
import qualified Data.Text as T

{- | Function which takes in the file path to a directory and a query string and returns the results, all via stdin/out. -}
query :: IO ()
query = do
    -- Get the file path and query from STDIN.
    input <- getContents
    let
        inputLines = lines input
        (path, q') =
            case inputLines of
                (piss:qi:_) -> (piss, qi)
                _ -> ("src", "ryvm")
        q = T.pack q'
    
    -- Get the search results
    results <- getSearchResults Nothing q path path
    
    let resultsTsv = searchResultsToTsv results
    -- Print each line to stdout
    mapM_ (putStrLn . T.unpack) (T.lines resultsTsv)