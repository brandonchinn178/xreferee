{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module XReferee.SearchResultSpec (spec) where

import Control.Exception (onException)
import Control.Monad (forM_)
import Data.Map qualified as Map
import Skeletest
import Skeletest.Predicate qualified as P
import System.Directory (
  createDirectoryIfMissing,
  withCurrentDirectory,
 )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process qualified as Process
import XReferee.SearchResult (
  LabelLoc (..),
  SearchResult (..),
  findRefsFromGit,
 )
import XReferee.TestUtils (anchor, defaultOpts)

spec :: Spec
spec = do
  describe "findRefsFromGit" $ do
    it "finds references throughout git repo" $ do
      let files =
            [ ("python/a/b/foo_anchor.py", "FOO = 1 # @@(foo)")
            , ("javascript/c/d/foo_ref.js", "const FOO = 1 ^^(foo)")
            ]
      withGitRepo files $ do
        let expected =
              SearchResult
                { anchors = Map.fromList [(anchor "foo", [LabelLoc "python/a/b/foo_anchor.py" 1])]
                , references = Map.fromList [(anchor "foo", [LabelLoc "javascript/c/d/foo_ref.js" 1])]
                }
        findRefsFromGit defaultOpts `shouldSatisfy` P.returns (P.eq expected)

    it "finds references from subdirectory" $ do
      let files =
            [ ("python/a/b/foo_anchor.py", "FOO = 1 # @@(foo)")
            , ("javascript/c/d/foo_ref.js", "const FOO = 1 ^^(foo)")
            ]
      withGitRepo files $ do
        let expected =
              SearchResult
                { anchors = Map.fromList [(anchor "foo", [LabelLoc "python/a/b/foo_anchor.py" 1])]
                , references = Map.fromList [(anchor "foo", [LabelLoc "javascript/c/d/foo_ref.js" 1])]
                }
        withCurrentDirectory "python/a/b/" $
          findRefsFromGit defaultOpts `shouldSatisfy` P.returns (P.eq expected)

data GitRepo = GitRepo
  { dir :: FilePath
  , logFile :: FilePath
  }

withGitRepo :: [(FilePath, String)] -> IO a -> IO a
withGitRepo files action =
  withSystemTempDirectory "git.XXXX" $ \tmpdir -> do
    let gitdir = tmpdir </> "repo"
        gitlog = tmpdir </> "git.log"
    createDirectoryIfMissing True gitdir
    withCurrentDirectory gitdir . captureLogs gitlog $ do
      let repo = GitRepo{dir = gitdir, logFile = gitlog}
      runGit repo ["init"]
      forM_ files $ \(fp, s) -> addFile repo fp s
      runGit repo ["commit", "-m", "Initial commit", "--no-verify"]
      action
  where
    -- TODO: remove when skeletest captures stdout/stderr
    -- https://github.com/brandonchinn178/skeletest/issues/1
    captureLogs logFile f = f `onException` (readFile logFile >>= putStrLn)

addFile :: GitRepo -> FilePath -> String -> IO ()
addFile repo relpath content = do
  let fp = repo.dir </> relpath
  createDirectoryIfMissing True (takeDirectory fp)
  writeFile fp content
  runGit repo ["add", relpath]

runGit :: GitRepo -> [String] -> IO ()
runGit repo args = do
  (code, stdout, stderr) <- Process.readProcessWithExitCode "git" args ""
  appendFile repo.logFile stdout
  appendFile repo.logFile stderr
  case code of
    ExitSuccess -> pure ()
    ExitFailure n -> do
      fail $ "command exited with code " <> show n <> ": " <> show ("git" : args)
