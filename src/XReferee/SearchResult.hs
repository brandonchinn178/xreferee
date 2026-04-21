{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module XReferee.SearchResult (
  -- * Options
  SearchOpts (..),
  MarkerDelims (..),
  defaultDelims,

  -- * Search results
  findRefsFromGit,
  SearchResult (..),
  Anchor (..),
  Reference (..),
  Label (..),
  ColumnRange (..),
  ColNum,
  LabelLoc (..),

  -- * Internal API
  parseLabels,
) where

import Control.DeepSeq (NFData (..), ($!!))
import Control.Exception (evaluate)
import Control.Monad (guard, when)
import Data.Bitraversable (bitraverse)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS.Char8
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Records (HasField (..))
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.Process qualified as Process
import Text.Read (readMaybe)

data SearchOpts = SearchOpts
  { delims :: MarkerDelims
  , ignores :: [Text]
  , includeUntracked :: Bool
  }

data MarkerDelims = MarkerDelims
  { anchorStart :: Text
  , anchorEnd :: Text
  , refStart :: Text
  , refEnd :: Text
  }
  deriving (Show, Eq)

defaultDelims :: MarkerDelims
defaultDelims =
  MarkerDelims
    { anchorStart = "#(ref:"
    , anchorEnd = ")"
    , refStart = "@(ref:"
    , refEnd = ")"
    }

data SearchResult = SearchResult
  { delims :: MarkerDelims
  , anchors :: Map Anchor [LabelLoc]
  , references :: Map Reference [LabelLoc]
  }
  deriving (Show, Eq)

instance NFData SearchResult where
  rnf result = rnf result.anchors `seq` rnf result.references
instance Semigroup SearchResult where
  result1 <> result2 =
    SearchResult
      { delims = result1.delims
      , anchors = Map.unionWith (<>) result1.anchors result2.anchors
      , references = Map.unionWith (<>) result1.references result2.references
      }

newtype Anchor = Anchor Text
  deriving (Show, Eq, Ord, NFData)

newtype Reference = Reference Text
  deriving (Show, Eq, Ord, NFData)

class Label a where
  getLabel :: a -> Text
  renderLabel :: MarkerDelims -> a -> Text
instance Label Anchor where
  getLabel (Anchor s) = s
  renderLabel delims (Anchor s) = delims.anchorStart <> s <> delims.anchorEnd
instance Label Reference where
  getLabel (Reference s) = s
  renderLabel delims (Reference s) = delims.refStart <> s <> delims.refEnd

data LabelLoc = LabelLoc
  { filepath :: FilePath
  , lineNum :: Int
  , columnRange :: ColumnRange
  }
  deriving (Show, Eq, Ord)

type ColNum = Int

data ColumnRange = ColumnRange
  { start :: ColNum
  , end :: ColNum
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
          , ["--fixed-strings", "-e", opts.delims.anchorStart, "-e", opts.delims.refStart]
          , ["--"]
          , [":/"]
          , [":!" <> i | i <- opts.ignores]
          ]
  result <- streamProcLines "git" args (parseLine opts.delims)
  LBS.hPutStr IO.stderr result.stderr
  when (result.code /= ExitSuccess && (not . LBS.null) result.stderr) $
    -- TODO: Proper error?
    errorWithoutStackTrace "git grep failed"
  pure $
    case NonEmpty.nonEmpty result.stdout of
      Nothing ->
        SearchResult
          { delims = opts.delims
          , anchors = Map.empty
          , references = Map.empty
          }
      Just results -> sconcat results

parseLine :: MarkerDelims -> LazyByteString -> Maybe SearchResult
parseLine delims line = do
  -- Split on \NUL characters
  [filepath, lineNumStr, colNumStr, rest] <- pure $ LBS.split 0 line
  lineNum <- readMaybe $ LBS.Char8.unpack lineNumStr
  colNum <- readMaybe $ LBS.Char8.unpack colNumStr
  let (anchors, references) = parseLabels delims rest colNum
      mkLoc columnRange =
        LabelLoc
          { filepath = LBS.Char8.unpack filepath
          , lineNum
          , columnRange
          }
  pure
    SearchResult
      { delims
      , anchors = Map.fromListWith (<>) [(anchor, [mkLoc range]) | (anchor, range) <- anchors]
      , references = Map.fromListWith (<>) [(ref, [mkLoc range]) | (ref, range) <- references]
      }

parseLabels ::
  MarkerDelims ->
  LazyByteString ->
  ColNum ->
  ([(Anchor, ColumnRange)], [(Reference, ColumnRange)])
parseLabels delims s0 col0 =
  partitionUnfoldr parseSomeMarker $
    ParseState
      { str = LBS.drop (fromIntegral col0 - 1) s0
      , col = col0
      }
  where
    toLBS = LBS.fromStrict . Text.encodeUtf8
    toText = Text.decodeUtf8 . LBS.toStrict

    anchorStartBS = toLBS delims.anchorStart
    anchorEndBS = toLBS delims.anchorEnd
    refStartBS = toLBS delims.refStart
    refEndBS = toLBS delims.refEnd

    -- TODO: When anchorStart/refStart are customizable, make sure to validate
    -- that they're non-empty
    anchorStartChar = LBS.head anchorStartBS
    refStartChar = LBS.head refStartBS

    parseSomeMarker state = do
      let anchorIndex = LBS.elemIndex anchorStartChar state.str
          refIndex = LBS.elemIndex refStartChar state.str
      case minMaybe anchorIndex refIndex of
        -- No more matches; stop the loop
        Nothing -> Nothing
        -- Found an anchor or ref
        Just eIndex -> do
          case bitraverse (parseAnchor state) (parseRef state) eIndex of
            -- Successful parse; return
            Just eResult -> Just $ distributeEither eResult
            -- False positive; try again
            Nothing -> do
              let n = fromEither eIndex + 1
              parseSomeMarker (state.drop n)
      where
        fromEither = either id id

        distributeEither :: Either (a, c) (b, c) -> (Either a b, c)
        distributeEither = \case
          Left (a, c) -> (Left a, c)
          Right (b, c) -> (Right b, c)

    parseAnchor = parseMarker (Anchor, anchorStartBS, anchorEndBS)
    parseRef = parseMarker (Reference, refStartBS, refEndBS)
    parseMarker (f, start, end) state0 index = do
      let state1 = state0.drop index
      state2 <- state1.stripPrefix start
      (name, state3) <- state2.splitOnce end
      guard $ (not . LBS.null) name
      let marker = (f . toText) name
          range =
            ColumnRange
              { start = state1.col
              , end = state3.col - 1
              }
      Just ((marker, range), state3)

data ParseState = ParseState
  { str :: !LazyByteString
  , col :: !ColNum
  }
instance HasField "drop" ParseState (Int64 -> ParseState) where
  getField state n =
    ParseState
      { str = LBS.drop n state.str
      , col = state.col + fromIntegral n
      }
instance HasField "stripPrefix" ParseState (LazyByteString -> Maybe ParseState) where
  getField state pre = go <$> LBS.stripPrefix pre state.str
    where
      go str' =
        ParseState
          { str = str'
          , col = state.col + fromIntegral (LBS.length pre)
          }
instance HasField "splitOnce" ParseState (LazyByteString -> Maybe (LazyByteString, ParseState)) where
  getField state delim = go <$> splitOnce delim state.str
    where
      go (res, str') =
        let state' =
              ParseState
                { str = str'
                , col = state.col + fromIntegral (LBS.length res + LBS.length delim)
                }
         in (res, state')

{----- Utilities -----}

data StreamProcResult a = StreamProcResult
  { code :: ExitCode
  , stdout :: a
  , stderr :: LazyByteString
  }

streamProcLines ::
  (NFData a) =>
  FilePath ->
  [Text] ->
  (LazyByteString -> Maybe a) ->
  IO (StreamProcResult [a])
streamProcLines cmd args onStdoutLine = do
  let proc =
        (Process.proc cmd (map Text.unpack args))
          { Process.std_out = Process.CreatePipe
          , Process.std_err = Process.CreatePipe
          }
  Process.withCreateProcess proc $ \_ stdoutHandle stderrHandle ph -> do
    rawStdout <- maybe (pure "") LBS.hGetContents stdoutHandle
    stdout <- evaluate $!! mapMaybe onStdoutLine . LBS.Char8.lines $ rawStdout
    rawStderr <- maybe (pure "") LBS.hGetContents stderrHandle
    stderr <- evaluate $!! rawStderr
    code <- Process.waitForProcess ph
    pure
      StreamProcResult
        { code
        , stdout
        , stderr
        }

partitionUnfoldr :: (s -> Maybe (Either a b, s)) -> s -> ([a], [b])
partitionUnfoldr f =
  let go !as !bs !s =
        case f s of
          Just (Left a, s') -> go (a : as) bs s'
          Just (Right b, s') -> go as (b : bs) s'
          Nothing -> (as, bs)
   in go [] []

{- | Return the smaller of the two.

>>> minMaybe (Just 1) (Just 3) == Just (Left 1)
>>> minMaybe (Just 3) (Just 1) == Just (Right 1)
>>> minMaybe (Just 1) Nothing  == Just (Left 1)
>>> minMaybe Nothing Nothing   == Nothing
-}
minMaybe :: (Ord a) => Maybe a -> Maybe a -> Maybe (Either a a)
minMaybe = \cases
  (Just a) (Just b) -> Just $ if a < b then Left a else Right b
  (Just a) Nothing -> Just (Left a)
  Nothing (Just b) -> Just (Right b)
  Nothing Nothing -> Nothing

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
