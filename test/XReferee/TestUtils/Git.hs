module XReferee.TestUtils.Git (
  withGitRepo,
  GitFileState (..),
  withGitRepoAndFileStates,
) where

import Control.Exception (onException)
import Control.Monad (forM_, unless)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import System.Directory (
  createDirectoryIfMissing,
  withCurrentDirectory,
 )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process qualified as Process

withGitRepo :: [(FilePath, Text)] -> IO a -> IO a
withGitRepo files =
  withGitRepoAndFileStates $ files <&> \(relpath, content) -> (Tracked, relpath, content)

-- | Create a temporary git repo whose files are in the given states.
withGitRepoAndFileStates :: [(GitFileState, FilePath, Text)] -> IO a -> IO a
withGitRepoAndFileStates files action =
  withSystemTempDirectory "git.XXXX" $ \tmpdir -> do
    let gitdir = tmpdir </> "repo"
        gitlog = tmpdir </> "git.log"
        git = runGit gitlog
    createDirectoryIfMissing True gitdir
    withCurrentDirectory gitdir . captureLogs gitlog $ do
      git ["init"]
      -- Write every file to disk.
      forM_ files $ \(_, relpath, content) -> do
        let fp = gitdir </> relpath
        createDirectoryIfMissing True (takeDirectory fp)
        Text.writeFile fp content

      -- Commit the tracked files *before* any .gitignore exists, so that
      -- TrackedIgnored files stay tracked once they are later ignored.
      let trackedPaths = [p | (st, p, _) <- files, st `elem` [Tracked, TrackedIgnored]]
      forM_ trackedPaths $ \p -> git ["add", "--", p]
      git ["commit", "-m", "Initial commit", "--allow-empty", "--no-verify"]

      -- Now ignore the ignored files.
      let ignoredPaths = [p | (st, p, _) <- files, st `elem` [TrackedIgnored, UntrackedIgnored]]
      unless (null ignoredPaths) $ do
        Text.writeFile (gitdir </> ".gitignore") (T.unlines (T.pack <$> ignoredPaths))
        git ["add", "--", ".gitignore"]
        git ["commit", "-m", "Add .gitignore", "--no-verify"]
      action
  where
    captureLogs logFile f = f `onException` (readFile logFile >>= putStrLn)

data GitFileState
  = -- | committed, not ignored
    Tracked
  | -- | committed, then added to @.gitignore@ in a later commit
    TrackedIgnored
  | -- | on disk, never staged, not ignored
    Untracked
  | -- | on disk, never staged, ignored
    UntrackedIgnored
  deriving (Eq)

runGit :: FilePath -> [String] -> IO ()
runGit logFile args = do
  (code, stdout, stderr) <- Process.readProcessWithExitCode "git" args ""
  appendFile logFile stdout
  appendFile logFile stderr
  case code of
    ExitSuccess -> pure ()
    ExitFailure n ->
      fail $ "command exited with code " <> show n <> ": " <> show ("git" : args)
