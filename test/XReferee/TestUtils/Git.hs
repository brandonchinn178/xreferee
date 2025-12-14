module XReferee.TestUtils.Git (
  withGitRepo,
) where

import Control.Exception (onException)
import Control.Monad (forM_)
import System.Directory (
  createDirectoryIfMissing,
  withCurrentDirectory,
 )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process qualified as Process

withGitRepo :: [(FilePath, String)] -> IO a -> IO a
withGitRepo files action =
  withSystemTempDirectory "git.XXXX" $ \tmpdir -> do
    let gitdir = tmpdir </> "repo"
        gitlog = tmpdir </> "git.log"
        git = runGit gitlog
    createDirectoryIfMissing True gitdir
    withCurrentDirectory gitdir . captureLogs gitlog $ do
      git ["init"]
      forM_ files $ \(relpath, content) -> do
        let fp = gitdir </> relpath
        createDirectoryIfMissing True (takeDirectory fp)
        writeFile fp content
      git ["add", "."]
      git ["commit", "-m", "Initial commit", "--allow-empty", "--no-verify"]
      action
  where
    captureLogs logFile f = f `onException` (readFile logFile >>= putStrLn)

    runGit logFile args = do
      (code, stdout, stderr) <- Process.readProcessWithExitCode "git" args ""
      appendFile logFile stdout
      appendFile logFile stderr
      case code of
        ExitSuccess -> pure ()
        ExitFailure n -> do
          fail $ "command exited with code " <> show n <> ": " <> show ("git" : args)
