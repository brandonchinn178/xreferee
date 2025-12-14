{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module XReferee.SearchResult (
  SearchOpts (..),
  SearchResult (..),
  Label (..),
  LabelLoc (..),
  findRefsFromGit,
) where

import Control.Monad (guard, when)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

data SearchOpts = SearchOpts
  { ignores :: [Text]
  }

data SearchResult = SearchResult
  { anchors :: Map Label [LabelLoc]
  , references :: Map Label [LabelLoc]
  }
  deriving (Show, Eq)

data Label = Label
  { name :: Text
  , pretty :: Text
  }
  deriving (Show)

instance Eq Label where
  l1 == l2 = l1.name == l2.name
instance Ord Label where
  compare l1 l2 = compare l1.name l2.name

data LabelLoc = LabelLoc
  { filepath :: FilePath
  , lineNum :: Int
  }
  deriving (Show, Eq)

findRefsFromGit :: SearchOpts -> IO SearchResult
findRefsFromGit opts = do
  anchors <- findLabelsFromGit opts "#(ref:" ")"
  references <- findLabelsFromGit opts "@(ref:" ")"
  pure SearchResult{..}

findLabelsFromGit :: SearchOpts -> Text -> Text -> IO (Map Label [LabelLoc])
findLabelsFromGit opts markerStart markerEnd = do
  let args =
        concat
          [ ["grep"]
          , ["--full-name"]
          , ["--line-number"]
          , ["-I"] -- ignore binary files
          , ["--fixed-strings", Text.unpack markerStart]
          , ["--"]
          , [":/"]
          , [":!" <> Text.unpack i | i <- opts.ignores]
          ]
  (code, stdout, stderr) <- readProcessWithExitCode "git" args ""
  when (code /= ExitSuccess && (not . null) stderr) $
    -- TODO: Proper error?
    errorWithoutStackTrace ("git grep failed: " <> stderr)

  pure . Map.fromListWith (<>) . map parseLine . Text.lines . Text.pack $ stdout
  where
    parseLine line =
      -- TODO: Proper error?
      fromMaybe (errorWithoutStackTrace $ "got unexpected output from git grep: " <> show line) $ do
        filepath : lineNumStr : rest <- pure $ Text.splitOn ":" line
        lineNum <- readMaybe $ Text.unpack lineNumStr
        label <- parseLabel $ Text.intercalate ":" rest
        let loc =
              LabelLoc
                { filepath = Text.unpack filepath
                , lineNum
                }
        pure (label, [loc])

    parseLabel line = do
      let name = takeUntil markerEnd . dropUntil markerStart $ line
      guard $ (not . Text.null) name
      pure
        Label
          { name = name
          , pretty = markerStart <> name <> markerEnd
          }

    dropUntil delim s =
      let (_, s') = Text.breakOn delim s
       in fromMaybe s' $ Text.stripPrefix delim s'
    takeUntil delim s =
      fst $ Text.breakOn delim s
