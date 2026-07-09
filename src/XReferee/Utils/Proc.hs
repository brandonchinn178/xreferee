{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module XReferee.Utils.Proc (
  StreamProcResult (..),
  streamProcLines,
  runProc,
  chunkArgs,
) where

import Control.DeepSeq (NFData, ($!!))
import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS.Char8
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Exit (ExitCode)
import System.Process qualified as Process

data StreamProcResult a = StreamProcResult
  { code :: ExitCode
  , stdout :: a
  , stderr :: LazyByteString
  }

streamProcLines ::
  (NFData a) =>
  FilePath ->
  [Text] ->
  (LazyByteString -> IO a) ->
  IO (StreamProcResult [a])
streamProcLines cmd args onStdoutLine = do
  let proc =
        (Process.proc cmd (map Text.unpack args))
          { Process.std_out = Process.CreatePipe
          , Process.std_err = Process.CreatePipe
          }
  Process.withCreateProcess proc $ \_ stdoutHandle stderrHandle ph -> do
    rawStdout <- maybe (pure "") LBS.hGetContents stdoutHandle
    stdout <- (evaluate $!!) =<< mapM onStdoutLine (LBS.Char8.lines rawStdout)
    rawStderr <- maybe (pure "") LBS.hGetContents stderrHandle
    stderr <- evaluate $!! rawStderr
    code <- Process.waitForProcess ph
    pure
      StreamProcResult
        { code
        , stdout
        , stderr
        }

{- | Run a process and capture its stdout and stderr, fully forced.
Unlike 'streamProcLines' this realizes the entire stdout in memory
-}
runProc :: FilePath -> [Text] -> IO (StreamProcResult LazyByteString)
runProc cmd args = do
  let proc =
        (Process.proc cmd (map Text.unpack args))
          { Process.std_out = Process.CreatePipe
          , Process.std_err = Process.CreatePipe
          }
  Process.withCreateProcess proc $ \_ stdoutHandle stderrHandle ph -> do
    rawStdout <- maybe (pure "") LBS.hGetContents stdoutHandle
    stdout <- evaluate $!! rawStdout
    rawStderr <- maybe (pure "") LBS.hGetContents stderrHandle
    stderr <- evaluate $!! rawStderr
    code <- Process.waitForProcess ph
    pure
      StreamProcResult
        { code
        , stdout
        , stderr
        }

{- | Different platforms have limits on the maximum command line length.
This function splits a list of arguments into chunks that fit within that limit, so that
each chunk can be passed to a command line invocation without exceeding the limit on any platform.
-}
chunkArgs :: [Text] -> [NonEmpty Text]
chunkArgs = go
  where
    go :: [Text] -> [NonEmpty Text]
    go [] = []
    go (x : xs) =
      -- Start a chunk with the first argument, and fill it with as many arguments as possible.
      let (chunk, rest) = fillChunk (argLen x) (NonEmpty.singleton x) xs
       in chunk : go rest

    fillChunk :: Int -> NonEmpty Text -> [Text] -> (NonEmpty Text, [Text])
    fillChunk _ chunk [] = (NonEmpty.reverse chunk, [])
    fillChunk chunkSize chunk (y : ys)
      | chunkSize + argLen y > maxChunkBytes =
          -- The argument `y` doesn't fit in the current chunk, so we return.
          (NonEmpty.reverse chunk, y : ys)
      | otherwise =
          fillChunk (chunkSize + argLen y) (NonEmpty.cons y chunk) ys

    argLen :: Text -> Int
    argLen t = BS.length (Text.encodeUtf8 t) + 1 -- +1 for the arg separator

    {-
      Windows's `CreateProcessW` has a maximum command line length of 32,767 characters.
      https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessw
      The limit on Linux and macOS is much higher.
      So we use a conservative limit to ensure compatibility across platforms.
    -}
    maxChunkBytes = 28000
