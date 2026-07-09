{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Monad (when)
import Data.Text.IO qualified as Text
import Options.Applicative qualified as Opt
import System.Exit (exitFailure)
import XReferee.Report (makeReport, renderReport, reportFailure)
import XReferee.SearchResult (
  SearchOpts (..),
  defaultDelims,
  findRefsFromGit,
 )

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
      -- TODO: Customize delimiters
      -- https://github.com/brandonchinn178/xreferee/issues/11
      --
      -- TODO: When anchorStart/refStart are customizable, make sure to validate
      -- that they're non-empty
      let delims = defaultDelims
      ignores <-
        Opt.many . Opt.strOption . mconcat $
          [ Opt.long "ignore"
          , Opt.short 'I'
          , Opt.help "Git glob specs for paths to ignore"
          ]
      includeUntracked <-
        Opt.switch . mconcat $
          [ Opt.long "include-untracked"
          , Opt.short 'U'
          , Opt.help "Include git untracked non-ignored files in the search"
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
