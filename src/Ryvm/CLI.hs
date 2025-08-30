{-# LANGUAGE OverloadedStrings #-}
module Ryvm.CLI where

import Options.Applicative
import Ryvm.Search
import Ryvm.Text.Tsv (searchResultsToTsv)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Data.List.Split (splitOn)

data Options = Options
  { optStdin :: Bool
  , optIsRelative :: Bool
  , optExtWhitelist :: [String]
  , optLocation :: Maybe FilePath
  , optQuery :: Maybe String
  }

optionsParser :: Parser Options
optionsParser = Options
    <$> switch
        ( long "stdin"
       <> help "Read location and query from stdin" )
    <*> switch
        ( long "make-relative"
       <> help "Make result paths relative to the search location" )
    <*> (splitOn "," <$> strOption
        ( long "ext-whitelist"
       <> value "txt"
       <> showDefault
       <> help "Comma-separated list of file extensions to search" ))
    <*> optional (argument str (metavar "LOCATION"))
    <*> optional (argument str (metavar "QUERY"))

run :: IO ()
run = do
    opts <- execParser optsParserInfo
    (location, queryStr) <- getInput opts
    let query = T.pack queryStr
    results <- getSearchResults (optIsRelative opts) (optExtWhitelist opts) Nothing query location location
    let resultsTsv = searchResultsToTsv results
    mapM_ Data.Text.IO.putStrLn (T.lines resultsTsv)

getInput :: Options -> IO (FilePath, String)
getInput opts
    | optStdin opts = do
        input <- getContents
        let inputLines = lines input
        case inputLines of
            (p:q:_) -> return (p, q)
            _ -> error "stdin mode requires two lines: location and query"
    | otherwise =
        case (optLocation opts, optQuery opts) of
            (Just loc, Just q) -> return (loc, q)
            _ -> error "LOCATION and QUERY arguments are required when not using --stdin"


optsParserInfo :: ParserInfo Options
optsParserInfo = info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Search for files in a directory and rank them by relevance."
   <> header "ryvm - Rank You Very Much" )