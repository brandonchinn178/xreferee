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
import Control.DeepSeq (NFData (..), ($!!))
import Control.Exception (evaluate)
import Control.Monad (guard, when)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.IO qualified as TextL
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.Process qualified as Process
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

instance NFData SearchResult where
  rnf result = rnf result.anchors `seq` rnf result.references
instance Semigroup SearchResult where
  result1 <> result2 =
    SearchResult
      { anchors = Map.unionWith (<>) result1.anchors result2.anchors
      , references = Map.unionWith (<>) result1.references result2.references
      }
instance Monoid SearchResult where
  mempty = SearchResult mempty mempty

newtype Anchor = Anchor Text
  deriving (Show, Eq, Ord, NFData)

newtype Reference = Reference Text
  deriving (Show, Eq, Ord, NFData)

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

instance NFData LabelLoc where
  rnf loc = rnf loc.filepath `seq` rnf loc.lineNum

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
      proc =
        (Process.proc "git" args)
          { Process.std_out = Process.CreatePipe
          , Process.std_err = Process.CreatePipe
          }
  Process.withCreateProcess proc $ \_ stdoutHandle stderrHandle ph -> do
    stdout <- maybe (pure "") TextL.hGetContents stdoutHandle
    result <- evaluate $!! mconcat . map parseLine . TextL.lines $ stdout
    code <- Process.waitForProcess ph
    stderr <- maybe (pure "") TextL.hGetContents stderrHandle
    TextL.hPutStr IO.stderr stderr
    when (code /= ExitSuccess && (not . TextL.null) stderr) $
      -- TODO: Proper error?
      errorWithoutStackTrace "git grep failed"
    pure result
  where
    parseLine line = fromMaybe mempty $ do
      filepath : lineNumStr : rest <- pure $ TextL.splitOn ":" line
      lineNum <- readMaybe $ TextL.unpack lineNumStr
      let (anchors, references) = parseLabels $ TextL.intercalate ":" rest
          loc =
            LabelLoc
              { filepath = TextL.unpack filepath
              , lineNum
              }
      pure
        SearchResult
          { anchors = Map.fromListWith (<>) [(anchor, [loc]) | anchor <- anchors]
          , references = Map.fromListWith (<>) [(ref, [loc]) | ref <- references]
          }

parseLabels :: TextL.Text -> ([Anchor], [Reference])
parseLabels = parseStart [] []
  where
    parseStart anchors refs s0 =
      let (_, s1) = TextL.break (`elem` [Text.head anchorStart, Text.head refStart]) s0
       in case (Left <$> parseAnchor s1) <|> (Right <$> parseRef s1) of
            Just (Left (name, s2)) -> parseStart (Anchor (TextL.toStrict name) : anchors) refs s2
            Just (Right (name, s2)) -> parseStart anchors (Reference (TextL.toStrict name) : refs) s2
            Nothing
              | TextL.null s1 -> (anchors, refs)
              | otherwise -> parseStart anchors refs (TextL.drop 1 s1)

    parseAnchor = parseMarker anchorStart anchorEnd
    parseRef = parseMarker refStart refEnd
    parseMarker start end s0 = do
      s1 <- TextL.stripPrefix (TextL.fromStrict start) s0
      (name, s2) <- breakOn' (TextL.fromStrict end) s1
      guard $ (not . TextL.null) name
      pure (name, s2)

    -- Same as breakOn, except returns Nothing if the delim isn't found, and
    -- the snd string doesn't start with the delim.
    breakOn' delim = traverse (TextL.stripPrefix delim) . TextL.breakOn delim
