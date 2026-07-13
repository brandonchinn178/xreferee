{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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
  LineNum,
  ColNum,
  LabelLoc (..),

  -- * Internal API
  parseLabels,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (NFData (..))
import Control.Monad (guard, when)
import Data.Bitraversable (bitraverse)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS.Char8
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.Records (HasField (..))
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import Text.Read (readMaybe)
import XReferee.Utils.Proc (StreamProcResult (..), chunkArgs, runProc, streamProcLines)
import XReferee.Utils.Utf16 (utf16Length)

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

emptySearchResult :: MarkerDelims -> SearchResult
emptySearchResult delims =
  SearchResult
    { delims
    , anchors = mempty
    , references = mempty
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
  , lineNum :: LineNum
  , columnRange :: ColumnRange
  }
  deriving (Show, Eq, Ord)

-- | 1-based line number.
type LineNum = Int

{- | 1-based column number.

The column number is based on UTF-16 code units, which is how offsets
are calculated by default in the LSP protocol.
See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/#textDocuments
-}
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

{- | Find all refs and anchors in the current git repository.

* If 'includeUntracked' is 'False', this will scan only files tracked by git (whether or not they are gitignored).
* If it's 'True', untracked non-ignored files are also scanned.
-}
findRefsFromGit :: SearchOpts -> IO SearchResult
findRefsFromGit opts = do
  tracked <- findRefsInTracked opts
  untracked <-
    if opts.includeUntracked
      then findRefsInUntrackedNonIgnored opts
      else pure (emptySearchResult opts.delims)
  pure (tracked <> untracked)

findRefsInTracked :: SearchOpts -> IO SearchResult
findRefsInTracked opts =
  runGitGrep opts.delims trackedArgs
  where
    trackedArgs =
      concat
        [ gitGrepBaseArgs opts.delims
        , ["--"]
        , [":/"]
        , [":!" <> i | i <- opts.ignores]
        ]

{- | Find refs and anchors in untracked, non-ignored files.

We use `git ls-files` to enumerate the untracked, non-ignored files, and then run `git grep` on those files.
See a detailed explanation here: https://github.com/brandonchinn178/xreferee/issues/27
-}
findRefsInUntrackedNonIgnored :: SearchOpts -> IO SearchResult
findRefsInUntrackedNonIgnored opts = do
  files <- listUntrackedFiles opts
  -- NOTE: `git ls-files` can potentially return a long list of files, which we have to pass to `git grep`.
  -- However, all platforms have a limit on the maximum command line length.
  -- So we have to split the list of pathspecs into chunks that fit within that limit, and run `git grep` on each chunk.
  combineResults <$> mapConcurrently (runGitGrep delims . untrackedArgs) (chunkArgs files)
  where
    delims = opts.delims

    combineResults :: [SearchResult] -> SearchResult
    combineResults results = maybe (emptySearchResult delims) sconcat (NonEmpty.nonEmpty results)

    -- NOTE: `pathspecs` must be `NonEmpty`, otherwise `git grep` will revert to
    -- its default behavior and also scan tracked files.
    untrackedArgs :: NonEmpty Text -> [Text]
    untrackedArgs pathspecs =
      concat
        [ -- Interpret filepaths as literal pathspecs, so filenames containing
          -- glob metacharacters (e.g. `[id].tsx`) are matched literally and not as a glob pattern.
          ["--literal-pathspecs"]
        , gitGrepBaseArgs delims
        , ["--untracked"] -- Without this, `git grep` ignores untracked files
        , ["--"]
        , NonEmpty.toList pathspecs
        ]

{- | List untracked, non-ignored files.
Filepaths will be relative to the current working directory (not necessarily the repo root).
-}
listUntrackedFiles :: SearchOpts -> IO [Text]
listUntrackedFiles opts = do
  result <- runProc "git" args
  LBS.hPutStr IO.stderr result.stderr
  when (result.code /= ExitSuccess && (not . LBS.null) result.stderr) $
    -- TODO: Proper error - https://github.com/brandonchinn178/xreferee/issues/4
    errorWithoutStackTrace "git ls-files failed"
  pure $
    result.stdout
      -- Split the lines by the NUL byte `\0`
      & LBS.split 0
      & filter (not . LBS.null)
      <&> LBS.toStrict
      <&> Text.decodeUtf8
  where
    args =
      concat
        [
          [ "ls-files"
          , "-z" -- Do not quote filenames with "unusual" characters. Output the filenames verbatim, separated by NUL bytes (i.e. `\0`).
          , "--others" -- List only untracked files...
          , "--exclude-standard" -- ... and apply the standard exclusion rules (e.g. .gitignore)
          ]
        , ["--"]
        , [":/"]
        , [":!" <> i | i <- opts.ignores]
        ]

-- | Run a single @git grep@ invocation and parse its output into a 'SearchResult'.
runGitGrep :: MarkerDelims -> [Text] -> IO SearchResult
runGitGrep delims args = do
  result <-
    streamProcLines "git" args $ \line -> do
      case extractGrepParts line of
        Nothing -> do
          LBS.Char8.hPutStrLn IO.stderr $ "[WARN] Found line in unexpected format: " <> line
          pure $ emptySearchResult delims
        Just (filepath, lineNum, match) -> do
          pure $ toSearchResult delims filepath lineNum match
  LBS.hPutStr IO.stderr result.stderr
  when (result.code /= ExitSuccess && (not . LBS.null) result.stderr) $
    -- TODO: Proper error - https://github.com/brandonchinn178/xreferee/issues/4
    errorWithoutStackTrace "git grep failed"
  pure $
    case NonEmpty.nonEmpty result.stdout of
      Nothing -> emptySearchResult delims
      Just results -> sconcat results
  where
    extractGrepParts line = do
      [filepathStr, lineNumStr, match] <- pure $ LBS.split 0 line
      let filepath = LBS.Char8.unpack filepathStr
      lineNum <- readMaybe $ LBS.Char8.unpack lineNumStr
      Just (filepath, lineNum, match)

-- | Shared @git grep@ flags and search patterns.
gitGrepBaseArgs :: MarkerDelims -> [Text]
gitGrepBaseArgs delims =
  concat
    [ ["grep"]
    , ["-z", "--full-name", "--line-number"]
    , ["-I"] -- ignore binary files
    , ["--fixed-strings"]
    , ["-e", delims.anchorStart]
    , ["-e", delims.refStart]
    ]

toSearchResult ::
  MarkerDelims ->
  FilePath ->
  LineNum ->
  LazyByteString ->
  SearchResult
toSearchResult delims filepath lineNum line =
  SearchResult
    { delims
    , anchors = toLabelMap anchors
    , references = toLabelMap references
    }
  where
    (anchors, references) = parseLabels delims line
    toLabelMap markers =
      Map.fromListWith (<>) $
        [ (marker, [loc])
        | (marker, range) <- markers
        , let loc =
                LabelLoc
                  { filepath
                  , lineNum
                  , columnRange = range
                  }
        ]

parseLabels ::
  MarkerDelims ->
  LazyByteString ->
  ([(Anchor, ColumnRange)], [(Reference, ColumnRange)])
parseLabels delims s0 = partitionUnfoldr parseSomeMarker ParseState{str = s0, col = 1}
  where
    toLBS = LBS.fromStrict . Text.encodeUtf8
    toText = Text.decodeUtf8 . LBS.toStrict

    anchorStartBS = toLBS delims.anchorStart
    anchorEndBS = toLBS delims.anchorEnd
    refStartBS = toLBS delims.refStart
    refEndBS = toLBS delims.refEnd

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
    let (before, after) = LBS.splitAt n state.str
     in ParseState
          { str = after
          , col = state.col + utf16Length before
          }
instance HasField "stripPrefix" ParseState (LazyByteString -> Maybe ParseState) where
  getField state pre = go <$> LBS.stripPrefix pre state.str
    where
      go str' =
        ParseState
          { str = str'
          , col = state.col + utf16Length pre
          }
instance HasField "splitOnce" ParseState (LazyByteString -> Maybe (LazyByteString, ParseState)) where
  getField state delim = go <$> splitOnce delim state.str
    where
      go (res, str') =
        let state' =
              ParseState
                { str = str'
                , col = state.col + utf16Length res + utf16Length delim
                }
         in (res, state')

{----- Utilities -----}

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
