{-# LANGUAGE OverloadedRecordDot #-}

module XReferee.IntegrationSpec (spec) where

import Control.Monad (forM_)
import Data.Text qualified as Text
import Skeletest
import XReferee.Report (makeReport, renderReport, reportFailure)
import XReferee.SearchResult (findRefsFromGit)
import XReferee.TestUtils.API (defaultOpts)
import XReferee.TestUtils.Fixtures (getGitFixtures)
import XReferee.TestUtils.Fixtures qualified as Fixture
import XReferee.TestUtils.Git (withGitRepo)

spec :: Spec
spec = do
  describe "fixture data" $ do
    it "passes" $ do
      fixtures <- getGitFixtures
      forM_ fixtures $ \(fixturePath, loadFixture) -> do
        fixture <- loadFixture
        withGitRepo fixture.files $ do
          result <- findRefsFromGit defaultOpts
          let report = makeReport result
          context fixturePath . context (Text.unpack $ renderReport report) $
            reportFailure report `shouldBe` False
