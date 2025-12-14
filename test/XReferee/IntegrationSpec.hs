module XReferee.IntegrationSpec (spec) where

import Control.Monad (forM_)
import Data.Text qualified as Text
import Skeletest
import XReferee.Report (makeReport, renderReport, reportFailure)
import XReferee.SearchResult (findRefsFromGit)
import XReferee.TestUtils.API (defaultOpts)
import XReferee.TestUtils.Fixtures (getGitFixtures)
import XReferee.TestUtils.Git (withGitRepo)

spec :: Spec
spec = do
  describe "fixture data" $ do
    it "passes" $ do
      fixtures <- getGitFixtures
      forM_ fixtures $ \(fixture, loadFixture) -> do
        files <- loadFixture
        withGitRepo files $ do
          result <- findRefsFromGit defaultOpts
          let report = makeReport result
          context fixture . context (Text.unpack $ renderReport report) $
            reportFailure report `shouldBe` False
