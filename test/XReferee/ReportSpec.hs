{-# LANGUAGE OverloadedStrings #-}

module XReferee.ReportSpec (spec) where

import Data.Map qualified as Map
import Skeletest
import Skeletest.Predicate qualified as P
import XReferee.Report (
  makeReport,
  renderReport,
  reportFailure,
 )
import XReferee.SearchResult (
  Anchor (..),
  ColumnRange (..),
  LabelLoc (..),
  Reference (..),
  SearchResult (..),
 )

dummyColumnRange :: ColumnRange
dummyColumnRange = ColumnRange 0 0

spec :: Spec
spec = do
  describe "renderReport" $ do
    it "renders a full report" $ do
      let result =
            SearchResult
              { anchors =
                  Map.fromList
                    [ (Anchor "foo", [LabelLoc "foo_anchor.py" 1 dummyColumnRange])
                    , (Anchor "unused", [LabelLoc "unused.py" 2 dummyColumnRange])
                    , (Anchor "dup", [LabelLoc "dup1.py" 3 dummyColumnRange, LabelLoc "dup2.py" 4 dummyColumnRange])
                    ]
              , references =
                  Map.fromList
                    [ (Reference "foo", [LabelLoc "foo_ref.py" 1 dummyColumnRange])
                    , (Reference "broken", [LabelLoc "broken.py" 2 dummyColumnRange])
                    , (Reference "dup", [LabelLoc "dup1.py" 3 dummyColumnRange, LabelLoc "dup2.py" 4 dummyColumnRange])
                    ]
              }
      (renderReport . makeReport) result `shouldSatisfy` P.matchesSnapshot

  describe "reportFailure" $ do
    it "reports fatal errors" $ do
      let result =
            SearchResult
              { anchors = Map.fromList []
              , references =
                  Map.fromList
                    [ (Reference "broken", [LabelLoc "broken.py" 2 dummyColumnRange])
                    ]
              }
      (reportFailure . makeReport) result `shouldBe` True

    it "ignores warnings" $ do
      let result =
            SearchResult
              { anchors =
                  Map.fromList
                    [ (Anchor "unused", [LabelLoc "unused.py" 1 dummyColumnRange])
                    ]
              , references = Map.fromList []
              }
      (reportFailure . makeReport) result `shouldBe` False
