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

import Control.Monad (when)
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

  pure . Map.fromListWith (<>) . concatMap parseLine . Text.lines . Text.pack $ stdout
  where
    parseLine line = fromMaybe [] $ do
      filepath : lineNumStr : rest <- pure $ Text.splitOn ":" line
      lineNum <- readMaybe $ Text.unpack lineNumStr
      let labels = parseLabels $ Text.intercalate ":" rest
          loc =
            LabelLoc
              { filepath = Text.unpack filepath
              , lineNum
              }
      pure [(fromLabel label, [loc]) | label <- labels]

    parseLabels line =
      let parseStart s =
            case breakOn' markerStart s of
              Just (_, s') -> parseLabel s'
              Nothing -> []
          parseLabel s =
            case breakOn' markerEnd s of
              Just (label, s') -> label : parseStart s'
              Nothing -> []
       in parseStart line

    -- Same as breakOn, except returns Nothing if the delim isn't found, and
    -- the snd string doesn't start with the delim.
    breakOn' delim = traverse (Text.stripPrefix delim) . Text.breakOn delim
