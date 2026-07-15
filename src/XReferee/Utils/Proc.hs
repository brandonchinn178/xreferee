{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module XReferee.Utils.Proc (
  StreamProcResult (..),
  withRunProc,
) where

import Control.DeepSeq (NFData, ($!!))
import Control.Exception (evaluate)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit (ExitCode)
import System.Process qualified as Process

data StreamProcResult a = StreamProcResult
  { code :: ExitCode
  , stdout :: a
  , stderr :: LazyByteString
  }

-- | Run a process, passing its raw stdout through the given callback to produce the result.
withRunProc ::
  (NFData a) =>
  FilePath ->
  [Text] ->
  (LazyByteString -> IO a) ->
  IO (StreamProcResult a)
withRunProc cmd args onStdout = do
  let proc =
        (Process.proc cmd (map Text.unpack args))
          { Process.std_out = Process.CreatePipe
          , Process.std_err = Process.CreatePipe
          }
  Process.withCreateProcess proc $ \_ stdoutHandle stderrHandle ph -> do
    rawStdout <- maybe (pure "") LBS.hGetContents stdoutHandle
    stdout <- (evaluate $!!) =<< onStdout rawStdout
    rawStderr <- maybe (pure "") LBS.hGetContents stderrHandle
    stderr <- evaluate $!! rawStderr
    code <- Process.waitForProcess ph
    pure
      StreamProcResult
        { code
        , stdout
        , stderr
        }
