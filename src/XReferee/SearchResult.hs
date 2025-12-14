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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.IO qualified as TextL
import Data.Void (Void)
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.Process qualified as Process
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as M

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
    result <- evaluate $!! parseOutput stdout
    code <- Process.waitForProcess ph
    stderr <- maybe (pure "") TextL.hGetContents stderrHandle
    TextL.hPutStr IO.stderr stderr
    when (code /= ExitSuccess && (not . TextL.null) stderr) $
      -- TODO: Proper error?
      errorWithoutStackTrace "git grep failed"
    pure result
  where
    parseOutput input =
      case runParser (mconcat <$> M.sepBy parseLine M.newline) input of
        Right result -> result
        -- TODO: Proper error?
        Left e -> errorWithoutStackTrace $ "git grep had invalid output: " <> e
    parseLine = (<|> pure mempty) $ do
      filepath <- TextL.unpack <$> M.takeWhile1P Nothing (/= ':')
      _ <- M.string ":"
      lineNum <- M.decimal
      _ <- M.string ":"
      (anchors, references) <- parseLabels
      let loc =
            LabelLoc
              { filepath
              , lineNum
              }
      pure
        SearchResult
          { anchors = Map.fromListWith (<>) [(anchor, [loc]) | anchor <- anchors]
          , references = Map.fromListWith (<>) [(ref, [loc]) | ref <- references]
          }

type Parser = M.Parsec Void TextL.Text

runParser :: Parser a -> TextL.Text -> Either String a
runParser p input =
  case M.runParser p "" input of
    Right x -> Right x
    Left e -> Left $ M.errorBundlePretty e

parseLabels :: Parser ([Anchor], [Reference])
parseLabels = parseStart [] []
  where
    markerStarts = [Text.head anchorStart, Text.head refStart]

    parseStart anchors refs = do
      _ <- M.takeWhileP Nothing (`notElem` '\n' : markerStarts)
      M.choice
        [ do
            name <- parseMarker anchorStart anchorEnd
            parseStart (Anchor (TextL.toStrict name) : anchors) refs
        , do
            name <- parseMarker refStart refEnd
            parseStart anchors (Reference (TextL.toStrict name) : refs)
        , do
            _ <- M.anySingleBut '\n'
            parseStart anchors refs
        , pure (anchors, refs)
        ]

    parseMarker start end = do
      _ <- M.string (TextL.fromStrict start)
      name <- TextL.pack <$> M.manyTill M.anySingle (M.string (TextL.fromStrict end))
      guard $ (not . TextL.null) name
      pure name
