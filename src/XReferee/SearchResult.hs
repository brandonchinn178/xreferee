{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module XReferee.SearchResult (
  SearchOpts (..),
  SearchResult (..),
  Anchor (..),
  Reference (..),
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

-- Customize? https://github.com/brandonchinn178/xreferee/issues/11
anchorStart, anchorEnd, refStart, refEnd :: Text
(anchorStart, anchorEnd) = ("#(ref:", ")")
(refStart, refEnd) = ("@(ref:", ")")

data SearchResult = SearchResult
  { anchors :: Map Anchor [LabelLoc]
  , references :: Map Reference [LabelLoc]
  }
  deriving (Show, Eq)

newtype Anchor = Anchor Text
  deriving (Show, Eq, Ord)

newtype Reference = Reference Text
  deriving (Show, Eq, Ord)

class Label a where
  fromLabel :: Text -> a
  toLabel :: a -> Text
  renderLabel :: a -> Text
instance Label Anchor where
  fromLabel = Anchor
  toLabel (Anchor s) = s
  renderLabel (Anchor s) = anchorStart <> s <> anchorEnd
instance Label Reference where
  fromLabel = Reference
  toLabel (Reference s) = s
  renderLabel (Reference s) = refStart <> s <> refEnd

data LabelLoc = LabelLoc
  { filepath :: FilePath
  , lineNum :: Int
  }
  deriving (Show, Eq)

findRefsFromGit :: SearchOpts -> IO SearchResult
findRefsFromGit opts = do
  anchors <- findLabelsFromGit opts anchorStart anchorEnd
  references <- findLabelsFromGit opts refStart refEnd
  pure SearchResult{..}

findLabelsFromGit :: (Label a, Ord a) => SearchOpts -> Text -> Text -> IO (Map a [LabelLoc])
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
      pure $ fromLabel name

    dropUntil delim s =
      let (_, s') = Text.breakOn delim s
       in fromMaybe s' $ Text.stripPrefix delim s'
    takeUntil delim s =
      fst $ Text.breakOn delim s
