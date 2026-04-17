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
  ColumnRange (..),
  LabelLoc (..),
  findRefsFromGit,
  parseLabels,
) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..), ($!!))
import Control.Exception (evaluate)
import Control.Monad (guard, when)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS.Char8
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.Process qualified as Process
import Text.Read (readMaybe)

data SearchOpts = SearchOpts
  { ignores :: [Text]
  , includeUntracked :: Bool
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
  , columnRange :: ColumnRange
  }
  deriving (Show, Eq, Ord)

data ColumnRange = ColumnRange
  { start :: Int
  , end :: Int
  }
  deriving (Show, Eq, Ord)

instance NFData ColumnRange where
  rnf (ColumnRange start end) = rnf start `seq` rnf end

instance NFData LabelLoc where
  rnf loc = rnf loc.filepath `seq` rnf loc.lineNum `seq` rnf loc.columnRange

findRefsFromGit :: SearchOpts -> IO SearchResult
findRefsFromGit opts = do
  let args =
        concat
          [ ["grep"]
          , ["-z", "--full-name", "--line-number", "--column"]
          , ["-I"] -- ignore binary files
          , ["--untracked" | opts.includeUntracked] -- include untracked files
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
    stdout <- maybe (pure "") LBS.hGetContents stdoutHandle
    result <- evaluate $!! mconcat . map parseLine . LBS.Char8.lines $ stdout
    code <- Process.waitForProcess ph
    stderr <- maybe (pure "") LBS.hGetContents stderrHandle
    LBS.hPutStr IO.stderr stderr
    when (code /= ExitSuccess && (not . LBS.null) stderr) $
      -- TODO: Proper error?
      errorWithoutStackTrace "git grep failed"
    pure result
  where
    parseLine line = fromMaybe mempty $ do
      -- Split on \NUL characters
      [filepath, lineNumStr, colNumStr, rest] <- pure $ LBS.split 0 line
      lineNum <- readMaybe $ LBS.Char8.unpack lineNumStr
      colNum <- readMaybe $ LBS.Char8.unpack colNumStr
      let (anchors, references) = parseLabels rest colNum
          mkLoc columnRange =
            LabelLoc
              { filepath = LBS.Char8.unpack filepath
              , lineNum
              , columnRange
              }
      pure
        SearchResult
          { anchors = Map.fromListWith (<>) [(anchor, [mkLoc range]) | (anchor, range) <- anchors]
          , references = Map.fromListWith (<>) [(ref, [mkLoc range]) | (ref, range) <- references]
          }

-- | Parse all labels from the given text.
parseLabels ::
  -- | The text to parse for labels.
  LazyByteString ->
  -- | The column number of the first label in the input text. Used to calculate the column numbers for the labels.
  Int ->
  ([(Anchor, ColumnRange)], [(Reference, ColumnRange)])
parseLabels text col =
  parseSomeMarker
    []
    []
    (LBS.drop (fromIntegral col - 1) text)
    col
  where
    markerStarts = map (LBS.head . toLBS) [anchorStart, refStart]
    toLBS = LBS.fromStrict . Text.encodeUtf8
    toText = Text.decodeUtf8 . LBS.toStrict

    parseSomeMarker ::
      [(Anchor, ColumnRange)] ->
      [(Reference, ColumnRange)] ->
      LazyByteString ->
      Int ->
      ([(Anchor, ColumnRange)], [(Reference, ColumnRange)])
    parseSomeMarker anchors refs s0 col0 =
      -- Remove the prefix before the next marker, and update the column number accordingly.
      let (prefix, s1) = LBS.break (`elem` markerStarts) s0
          col1 = col0 + fromIntegral (LBS.length prefix)
       in case (Left <$> parseAnchor s1) <|> (Right <$> parseRef s1) of
            Just (Left (name, s2)) ->
              let markerLen = Text.length anchorStart + fromIntegral (LBS.length name) + Text.length anchorEnd
                  columnRange = ColumnRange{start = col1, end = col1 + markerLen - 1}
                  -- Advance the column number to match the remaining string `s2`
                  col2 = col1 + markerLen
               in parseSomeMarker ((Anchor (toText name), columnRange) : anchors) refs s2 col2
            Just (Right (name, s2)) ->
              let markerLen = Text.length refStart + fromIntegral (LBS.length name) + Text.length refEnd
                  columnRange = ColumnRange{start = col1, end = col1 + markerLen - 1}
                  col2 = col1 + markerLen
               in parseSomeMarker anchors ((Reference (toText name), columnRange) : refs) s2 col2
            Nothing
              | LBS.null s1 -> (anchors, refs)
              | otherwise -> parseSomeMarker anchors refs (LBS.drop 1 s1) (col1 + 1)

    parseAnchor = parseMarker anchorStart anchorEnd
    parseRef = parseMarker refStart refEnd
    parseMarker start end s0 = do
      s1 <- LBS.stripPrefix (toLBS start) s0
      (name, s2) <- splitOnce (toLBS end) s1
      guard $ (not . LBS.null) name
      pure (name, s2)

{----- Utilities -----}

{- | Split on the given delimiter

>>> splitOnce "::" "a" == Nothing
>>> splitOnce "::" "a::b::c" == Just ("a", "b::c")
-}
splitOnce :: LazyByteString -> LazyByteString -> Maybe (LazyByteString, LazyByteString)
splitOnce delim =
  let delimChar = LBS.head delim
      go s =
        case LBS.elemIndex delimChar s of
          -- Delimiter not found
          Nothing -> Nothing
          Just i
            -- Found delimiter
            | Just s' <- LBS.stripPrefix delim (LBS.drop i s) -> Just (LBS.take i s, s')
            -- False positive, try again
            | otherwise -> go (LBS.drop (i + 1) s)
   in go
