{-# LANGUAGE OverloadedRecordDot #-}

module XReferee.IntegrationSpec (spec) where

import Control.Monad (forM_)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Skeletest
import XReferee.Report (makeReport, renderReport, reportFailure)
import XReferee.SearchResult (
  Anchor (..),
  LabelLoc (..),
  Reference (..),
  SearchResult (..),
  findRefsFromGit,
 )
import XReferee.TestUtils.API (defaultOpts)
import XReferee.TestUtils.Fixtures (Loc (..), getGitFixtures)
import XReferee.TestUtils.Fixtures qualified as Fixture
import XReferee.TestUtils.Git (withGitRepo)

spec :: Spec
spec = do
  describe "fixture data" $ do
    it "passes" $ do
      fixtures <- getGitFixtures
      forM_ fixtures $ \(fixturePath, loadFixture) -> do
        fixture <- loadFixture
        let expectedAnchors = Map.mapKeys Anchor fixture.anchors
            expectedRefs = Map.mapKeys Reference fixture.refs

        withGitRepo fixture.files $ do
          result <- findRefsFromGit defaultOpts
          context fixturePath $ do
            -- Manually iterate to show smaller errors
            forM_ (Map.toList expectedAnchors) $ \(anchor, loc) ->
              context (show anchor) $
                Map.findWithDefault [] anchor result.anchors `shouldMatchLocs` [loc]
            forM_ (Map.toList expectedRefs) $ \(ref, locs) ->
              context (show ref) $
                Map.findWithDefault [] ref result.references `shouldMatchLocs` locs
          let report = makeReport result
          context fixturePath . context (Text.unpack $ renderReport report) $
            reportFailure report `shouldBe` False
  where
    shouldMatchLocs :: [LabelLoc] -> [Loc] -> IO ()
    labelLocs `shouldMatchLocs` locs = Set.fromList (toLoc <$> labelLocs) `shouldBe` Set.fromList locs
      where
        -- Ignore `LabelLoc.columnRange` for the purpose of integration tests,
        -- since column ranges are not part of the report.
        toLoc (LabelLoc file lineNum _columnRange) = Loc{file, Fixture.lineNum}
