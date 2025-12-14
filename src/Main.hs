{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Monad (when)
import Data.Text.IO qualified as Text
import Options.Applicative qualified as Opt
import System.Exit (exitFailure)
import XReferee.Report (makeReport, renderReport, reportFailure)
import XReferee.SearchResult (SearchOpts (..), findRefsFromGit)

{----- CLI Options -----}

data CLIOptions = CLIOptions
  { searchOpts :: SearchOpts
  }

cliOptions :: Opt.ParserInfo CLIOptions
cliOptions =
  Opt.info (Opt.helper <*> parseOptions) . mconcat $
    [ Opt.fullDesc
    , Opt.header "xreferee: Validate cross references"
    ]
  where
    parseOptions = do
      searchOpts <- parseSearchOpts
      pure CLIOptions{..}

    parseSearchOpts = do
      ignores <-
        Opt.many . Opt.strOption . mconcat $
          [ Opt.long "ignore"
          , Opt.short 'I'
          , Opt.help "Git glob specs for paths to ignore"
          ]
      pure SearchOpts{..}

{----- Entrypoint -----}

main :: IO ()
main = do
  cli <- Opt.execParser cliOptions
  searchResult <- findRefsFromGit cli.searchOpts
  let report = makeReport searchResult
  Text.putStr $ renderReport report
  when (reportFailure report) exitFailure
