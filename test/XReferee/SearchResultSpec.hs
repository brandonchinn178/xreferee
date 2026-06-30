{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module XReferee.SearchResultSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.Text.Encoding qualified as TE
import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range
import System.Directory (
  withCurrentDirectory,
 )
import XReferee.SearchResult (
  SearchOpts (..),
  SearchResult (..),
  findRefsFromGit,
 )
import XReferee.TestUtils.API (anchor, defaultOpts, loc', ref)
import XReferee.TestUtils.Git (withGitRepo)
import XReferee.Utils.Utf16 (utf16Length)

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
                { delims = defaultOpts.delims
                , anchors = Map.fromList [anchor "foo" [loc' "python/a/b/foo_anchor.py" 1 (11, 20)]]
                , references = Map.fromList [ref "foo" [loc' "javascript/c/d/foo_ref.js" 1 (15, 24)]]
                }
        findRefsFromGit defaultOpts `shouldSatisfy` P.returns (P.eq expected)

    it "finds references on same line" $ do
      let files =
            [ ("python/a/b/foo_anchor.py", "FOO = 1 # #(ref:foo) #(ref:foo) #(ref:foo2)")
            , ("javascript/c/d/foo_ref.js", "const FOO = 1 @(ref:foo) @(ref:foo) @(ref:foo2)")
            , ("mixed_anchor_first.sh", "FOO=1 # #(ref:mixed1) @(ref:mixed2)")
            , ("mixed_ref_first.sh", "FOO=1 # @(ref:mixed1) #(ref:mixed2)")
            ]
      withGitRepo files $ do
        let expected =
              SearchResult
                { delims = defaultOpts.delims
                , anchors =
                    Map.fromList
                      [ anchor "foo" [loc' "python/a/b/foo_anchor.py" 1 (11, 20), loc' "python/a/b/foo_anchor.py" 1 (22, 31)]
                      , anchor "foo2" [loc' "python/a/b/foo_anchor.py" 1 (33, 43)]
                      , anchor "mixed1" [loc' "mixed_anchor_first.sh" 1 (9, 21)]
                      , anchor "mixed2" [loc' "mixed_ref_first.sh" 1 (23, 35)]
                      ]
                , references =
                    Map.fromList
                      [ ref "foo" [loc' "javascript/c/d/foo_ref.js" 1 (15, 24), loc' "javascript/c/d/foo_ref.js" 1 (26, 35)]
                      , ref "foo2" [loc' "javascript/c/d/foo_ref.js" 1 (37, 47)]
                      , ref "mixed1" [loc' "mixed_ref_first.sh" 1 (9, 21)]
                      , ref "mixed2" [loc' "mixed_anchor_first.sh" 1 (23, 35)]
                      ]
                }
        findRefsFromGit defaultOpts `shouldSatisfy` P.returns (P.eq expected)

    it "handles ignores" $ do
      withGitRepo [("ignored/test.txt", "@(ref:broken)")] $ do
        let opts = defaultOpts{ignores = ["ignored/"]}
            expected =
              SearchResult
                { delims = opts.delims
                , anchors = mempty
                , references = mempty
                }
        findRefsFromGit opts `shouldSatisfy` P.returns (P.eq expected)

    it "handles files with special characters" $ do
      let files =
            [ ("foo:49:.txt", "#(ref:test1)")
            , ("foo\\.txt", "#(ref:test2)")
            ]
      withGitRepo files $ do
        let expected =
              SearchResult
                { delims = defaultOpts.delims
                , anchors =
                    Map.fromList
                      [ anchor "test1" [loc' "foo:49:.txt" 1 (1, 12)]
                      , anchor "test2" [loc' "foo\\.txt" 1 (1, 12)]
                      ]
                , references = mempty
                }
        findRefsFromGit defaultOpts `shouldSatisfy` P.returns (P.eq expected)

    it "counts columns as UTF-16 code units" $ do
      let files =
            -- "😀" is a single codepoint (U+1F600), but is not in the BMP (Basic Multilingual Plane),
            -- so it's encoded using 2 UTF-16 code units.
            [ ("emoji_anchor.py", "#(ref:fo😀o)")
            , {- The family emoji "👨‍👩‍👧‍👦" is a sequence of 7 codepoints:

                * U+1F468: 👨, 2 UTF-16 code units
                * U+200D: ZWJ (zero-width joiner), 1 UTF-16 code units
                * U+1F469: 👩, 2 UTF-16 code units
                * U+200D: ZWJ, 1 UTF-16 code units
                * U+1F467: 👧, 2 UTF-16 code units
                * U+200D: ZWJ, 1 UTF-16 code units
                * U+1F466: 👦, 2 UTF-16 code unitss

                Total: 11 UTF-16 code units
              -}
              ("family_anchor.py", "\x1F468\x200D\x1F469\x200D\x1F467\x200D\x1F466#(ref:bar)")
            ]
      withGitRepo files $ do
        let expected =
              SearchResult
                { delims = defaultOpts.delims
                , anchors =
                    Map.fromList
                      -- NOTE: columns are 1-based.
                      [ anchor "fo😀o" [loc' "emoji_anchor.py" 1 (1, 12)]
                      , anchor "bar" [loc' "family_anchor.py" 1 (12, 21)]
                      ]
                , references = mempty
                }
        findRefsFromGit defaultOpts `shouldSatisfy` P.returns (P.eq expected)

    it "finds references from subdirectory" $ do
      let files =
            [ ("python/a/b/foo_anchor.py", "FOO = 1 # #(ref:foo)")
            , ("javascript/c/d/foo_ref.js", "const FOO = 1 @(ref:foo)")
            ]
      withGitRepo files $ do
        let expected =
              SearchResult
                { delims = defaultOpts.delims
                , anchors = Map.fromList [anchor "foo" [loc' "python/a/b/foo_anchor.py" 1 (11, 20)]]
                , references = Map.fromList [ref "foo" [loc' "javascript/c/d/foo_ref.js" 1 (15, 24)]]
                }
        withCurrentDirectory "python/a/b/" $
          findRefsFromGit defaultOpts `shouldSatisfy` P.returns (P.eq expected)

  describe "utf16Length" $ do
    prop "matches reference implementation via encodeUtf16BE" $ do
      t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicodeAll
      let bs = LBS.fromStrict (TE.encodeUtf8 t)
      -- 1 UTF-16 code unit = 2 bytes, so we divide the length of the encoded
      -- bytes by 2 to get the expected number of UTF-16 code units.
      let expected = BS.length (TE.encodeUtf16BE t) `div` 2
      utf16Length bs `shouldBe` expected
