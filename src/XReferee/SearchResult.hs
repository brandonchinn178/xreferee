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

import Control.Applicative ((<|>))
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

instance Semigroup SearchResult where
  result1 <> result2 =
    SearchResult
      { anchors = Map.unionWith (<>) result1.anchors result2.anchors
      , references = Map.unionWith (<>) result1.references result2.references
      }
instance Monoid SearchResult where
  mempty = SearchResult mempty mempty

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
  let args =
        concat
          [ ["grep"]
          , ["--full-name", "--line-number"]
          , ["-I"] -- ignore binary files
          , ["--fixed-strings", "-e", Text.unpack anchorStart, "-e", Text.unpack refStart]
          , ["--"]
          , [":/"]
          , [":!" <> Text.unpack i | i <- opts.ignores]
          ]
  (code, stdout, stderr) <- readProcessWithExitCode "git" args ""
  when (code /= ExitSuccess && (not . null) stderr) $
    -- TODO: Proper error?
    errorWithoutStackTrace ("git grep failed: " <> stderr)

  pure . mconcat . map parseLine . Text.lines . Text.pack $ stdout
  where
    parseLine line = fromMaybe mempty $ do
      filepath : lineNumStr : rest <- pure $ Text.splitOn ":" line
      lineNum <- readMaybe $ Text.unpack lineNumStr
      let (anchors, references) = parseLabels $ Text.intercalate ":" rest
          loc =
            LabelLoc
              { filepath = Text.unpack filepath
              , lineNum
              }
      pure
        SearchResult
          { anchors = Map.fromListWith (<>) [(anchor, [loc]) | anchor <- anchors]
          , references = Map.fromListWith (<>) [(ref, [loc]) | ref <- references]
          }

parseLabels :: Text -> ([Anchor], [Reference])
parseLabels = parseStart [] []
  where
    parseStart anchors refs s0 =
      let (_, s1) = Text.break (`elem` [Text.head anchorStart, Text.head refStart]) s0
       in case (Left <$> parseAnchor s1) <|> (Right <$> parseRef s1) of
            Just (Left (name, s2)) -> parseStart (Anchor name : anchors) refs s2
            Just (Right (name, s2)) -> parseStart anchors (Reference name : refs) s2
            Nothing
              | Text.null s1 -> (anchors, refs)
              | otherwise -> parseStart anchors refs (Text.drop 1 s1)

    parseAnchor = parseMarker anchorStart anchorEnd
    parseRef = parseMarker refStart refEnd
    parseMarker start end s0 = do
      s1 <- Text.stripPrefix start s0
      (name, s2) <- breakOn' end s1
      guard $ (not . Text.null) name
      pure (name, s2)

    -- Same as breakOn, except returns Nothing if the delim isn't found, and
    -- the snd string doesn't start with the delim.
    breakOn' delim = traverse (Text.stripPrefix delim) . Text.breakOn delim
