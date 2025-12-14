{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module XReferee.SearchResultSpec (spec) where

import Data.Map qualified as Map
import Skeletest
import Skeletest.Predicate qualified as P
import System.Directory (
  withCurrentDirectory,
 )
import XReferee.GitUtils (withGitRepo)
import XReferee.SearchResult (
  Anchor (..),
  LabelLoc (..),
  Reference (..),
  SearchOpts (..),
  SearchResult (..),
  findRefsFromGit,
 )
import XReferee.TestUtils (defaultOpts)

spec :: Spec
spec = do
  describe "findRefsFromGit" $ do
    it "finds references throughout git repo" $ do
      let files =
            [ ("python/a/b/foo_anchor.py", "FOO = 1 # #(ref:foo)")
            , ("javascript/c/d/foo_ref.js", "const FOO = 1 @(ref:foo)")
            ]
      withGitRepo files $ do
        let expected =
              SearchResult
                { anchors = Map.fromList [(Anchor "foo", [LabelLoc "python/a/b/foo_anchor.py" 1])]
                , references = Map.fromList [(Reference "foo", [LabelLoc "javascript/c/d/foo_ref.js" 1])]
                }
        findRefsFromGit defaultOpts `shouldSatisfy` P.returns (P.eq expected)

    it "finds references on same line" $ do
      let files =
            [ ("python/a/b/foo_anchor.py", "FOO = 1 # #(ref:foo) #(ref:foo2)")
            , ("javascript/c/d/foo_ref.js", "const FOO = 1 @(ref:foo) @(ref:foo2)")
            ]
      withGitRepo files $ do
        let expected =
              SearchResult
                { anchors =
                    Map.fromList
                      [ (Anchor "foo", [LabelLoc "python/a/b/foo_anchor.py" 1])
                      , (Anchor "foo2", [LabelLoc "python/a/b/foo_anchor.py" 1])
                      ]
                , references =
                    Map.fromList
                      [ (Reference "foo", [LabelLoc "javascript/c/d/foo_ref.js" 1])
                      , (Reference "foo2", [LabelLoc "javascript/c/d/foo_ref.js" 1])
                      ]
                }
        findRefsFromGit defaultOpts `shouldSatisfy` P.returns (P.eq expected)

    it "handles ignores" $ do
      withGitRepo [("ignored/test.txt", "@(ref:broken)")] $ do
        let opts = defaultOpts{ignores = ["ignored/"]}
            expected = SearchResult{anchors = mempty, references = mempty}
        findRefsFromGit opts `shouldSatisfy` P.returns (P.eq expected)

    it "finds references from subdirectory" $ do
      let files =
            [ ("python/a/b/foo_anchor.py", "FOO = 1 # #(ref:foo)")
            , ("javascript/c/d/foo_ref.js", "const FOO = 1 @(ref:foo)")
            ]
      withGitRepo files $ do
        let expected =
              SearchResult
                { anchors = Map.fromList [(Anchor "foo", [LabelLoc "python/a/b/foo_anchor.py" 1])]
                , references = Map.fromList [(Reference "foo", [LabelLoc "javascript/c/d/foo_ref.js" 1])]
                }
        withCurrentDirectory "python/a/b/" $
          findRefsFromGit defaultOpts `shouldSatisfy` P.returns (P.eq expected)
