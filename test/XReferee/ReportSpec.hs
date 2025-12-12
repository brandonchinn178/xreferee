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
  LabelLoc (..),
  SearchResult (..),
 )
import XReferee.TestUtils (anchor, ref)

spec :: Spec
spec = do
  describe "renderReport" $ do
    it "renders a full report" $ do
      let result =
            SearchResult
              { anchors =
                  Map.fromList
                    [ (anchor "foo", [LabelLoc "foo_anchor.py" 1])
                    , (anchor "unused", [LabelLoc "unused.py" 2])
                    , (anchor "dup", [LabelLoc "dup1.py" 3, LabelLoc "dup2.py" 4])
                    ]
              , references =
                  Map.fromList
                    [ (ref "foo", [LabelLoc "foo_ref.py" 1])
                    , (ref "broken", [LabelLoc "broken.py" 2])
                    , (ref "dup", [LabelLoc "dup1.py" 3, LabelLoc "dup2.py" 4])
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
                    [ (ref "broken", [LabelLoc "broken.py" 2])
                    ]
              }
      (reportFailure . makeReport) result `shouldBe` True

    it "ignores warnings" $ do
      let result =
            SearchResult
              { anchors =
                  Map.fromList
                    [ (anchor "unused", [LabelLoc "unused.py" 1])
                    ]
              , references = Map.fromList []
              }
      (reportFailure . makeReport) result `shouldBe` False
