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
  ColumnRange (..),
  LabelLoc (..),
  Reference (..),
  SearchResult (..),
  findRefsFromGit,
 )
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
        let expectedAnchors =
              Map.mapKeys Anchor
                . Map.map ((: []) . fromFixtureLoc)
                $ fixture.anchors
            expectedRefs =
              Map.mapKeys Reference
                . Map.map (map fromFixtureLoc)
                $ fixture.refs

        withGitRepo fixture.files $ do
          result <- findRefsFromGit defaultOpts
          context fixturePath $ do
            -- Manually iterate to show smaller errors
            forM_ (Map.toList expectedAnchors) $ \(anchor, locs) ->
              context (show anchor) $
                Set.fromList (toFileAndLine <$> Map.findWithDefault [] anchor result.anchors)
                  `shouldBe` Set.fromList (toFileAndLine <$> locs)
            forM_ (Map.toList expectedRefs) $ \(ref, locs) ->
              context (show ref) $
                Set.fromList (toFileAndLine <$> Map.findWithDefault [] ref result.references)
                  `shouldBe` Set.fromList (toFileAndLine <$> locs)
          let report = makeReport result
          context fixturePath . context (Text.unpack $ renderReport report) $
            reportFailure report `shouldBe` False
  where
    fromFixtureLoc loc =
      LabelLoc
        { filepath = loc.file
        , lineNum = loc.lineNum
        , columnRange = ColumnRange 0 0
        }

    -- Ignore `LabelLoc.columnRange` for the purpose of integration tests,
    -- since column ranges are not part of the report.
    toFileAndLine :: LabelLoc -> (FilePath, Int)
    toFileAndLine loc = (loc.filepath, loc.lineNum)
