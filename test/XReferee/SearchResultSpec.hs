{-# LANGUAGE DisambiguateRecordFields #-}
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
import XReferee.SearchResult (
  SearchOpts (..),
  SearchResult (..),
  findRefsFromGit,
 )
import XReferee.TestUtils.API (anchor, defaultOpts, loc', ref)
import XReferee.TestUtils.Git (
  GitFileState (..),
  withGitRepo,
  withGitRepoAndFileStates,
 )

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
      {-
        "😀" is a single codepoint (U+1F600), but is not in the BMP (Basic Multilingual Plane),
        so it's encoded using 2 UTF-16 code units.

        The family emoji "👨‍👩‍👧‍👦" is a sequence of 7 codepoints:
          * U+1F468: 👨, 2 UTF-16 code units
          * U+200D: ZWJ (zero-width joiner), 1 UTF-16 code units
          * U+1F469: 👩, 2 UTF-16 code units
          * U+200D: ZWJ, 1 UTF-16 code units
          * U+1F467: 👧, 2 UTF-16 code units
          * U+200D: ZWJ, 1 UTF-16 code units
          * U+1F466: 👦, 2 UTF-16 code unitss
        Total: 11 UTF-16 code units
      -}
      let files =
            [ ("emoji_anchor.py", "#(ref:fo😀o)")
            , ("family_anchor.py", "\x1F468\x200D\x1F469\x200D\x1F467\x200D\x1F466#(ref:bar)")
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

    it "searches tracked files (ignored or not), but not untracked files, by default" $ do
      let files =
            [ (Tracked, "a-tracked.md", "@(ref:a-tracked)")
            , (TrackedIgnored, "b-tracked-ignored.md", "@(ref:b-tracked-ignored)")
            , (Untracked, "c-untracked.md", "@(ref:c-untracked)")
            , (UntrackedIgnored, "d-untracked-ignored.md", "@(ref:d-untracked-ignored)")
            ]
      withGitRepoAndFileStates files $ do
        let expected =
              SearchResult
                { delims = defaultOpts.delims
                , anchors = mempty
                , references =
                    Map.fromList
                      [ ref "a-tracked" [loc' "a-tracked.md" 1 (1, 16)]
                      , ref "b-tracked-ignored" [loc' "b-tracked-ignored.md" 1 (1, 24)]
                      ]
                }
        findRefsFromGit defaultOpts `shouldSatisfy` P.returns (P.eq expected)

    it "also searches untracked, non-ignored files with includeUntracked" $ do
      let files =
            [ (Tracked, "a-tracked.md", "@(ref:a-tracked)")
            , (TrackedIgnored, "b-tracked-ignored.md", "@(ref:b-tracked-ignored)")
            , (Untracked, "c-untracked.md", "@(ref:c-untracked)")
            , (UntrackedIgnored, "d-untracked-ignored.md", "@(ref:d-untracked-ignored)")
            ]
      withGitRepoAndFileStates files $ do
        let expected =
              SearchResult
                { delims = defaultOpts.delims
                , anchors = mempty
                , references =
                    Map.fromList
                      [ ref "a-tracked" [loc' "a-tracked.md" 1 (1, 16)]
                      , ref "b-tracked-ignored" [loc' "b-tracked-ignored.md" 1 (1, 24)]
                      , ref "c-untracked" [loc' "c-untracked.md" 1 (1, 18)]
                      ]
                }
        findRefsFromGit defaultOpts{includeUntracked = True}
          `shouldSatisfy` P.returns (P.eq expected)

    it "applies `ignores` to both tracked and untracked files when includeUntracked is set" $ do
      let files =
            [ (Tracked, "keep/a.md", "@(ref:keep-tracked)")
            , (Tracked, "excluded/b.md", "@(ref:excluded-tracked)")
            , (Untracked, "keep/c.md", "@(ref:keep-untracked)")
            , (Untracked, "excluded/d.md", "@(ref:excluded-untracked)")
            ]
      withGitRepoAndFileStates files $ do
        let opts = defaultOpts{ignores = ["excluded/"], includeUntracked = True}
            expected =
              SearchResult
                { delims = opts.delims
                , anchors = mempty
                , references =
                    Map.fromList
                      [ ref "keep-tracked" [loc' "keep/a.md" 1 (1, 19)]
                      , ref "keep-untracked" [loc' "keep/c.md" 1 (1, 21)]
                      ]
                }
        findRefsFromGit opts `shouldSatisfy` P.returns (P.eq expected)

    it "treats untracked filenames as literal pathspecs, not globs" $ do
      -- `[id].tsx` is a perfectly valid untracked filename (e.g. a Next.js dynamic route).
      -- If it were to be passed to `git grep --untracked` as a glob, it would also match
      -- the tracked file `i.tsx`.
      -- As a result, both the `git grep` and the `git grep --untracked` passes would scan `i.tsx`,
      -- and the references in the file would be counted twice.
      -- This test ensures the string `[id].tsx` is treated as a literal, not as a glob.
      let files =
            [ (Tracked, "i.tsx", "@(ref:tracked-i)")
            , (Untracked, "[id].tsx", "@(ref:untracked-dynamic)")
            ]
      withGitRepoAndFileStates files $ do
        let expected =
              SearchResult
                { delims = defaultOpts.delims
                , anchors = mempty
                , references =
                    Map.fromList
                      [ ref "tracked-i" [loc' "i.tsx" 1 (1, 16)]
                      , ref "untracked-dynamic" [loc' "[id].tsx" 1 (1, 24)]
                      ]
                }
        findRefsFromGit defaultOpts{includeUntracked = True}
          `shouldSatisfy` P.returns (P.eq expected)

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

    it "finds untracked references from a subdirectory" $ do
      -- Untracked files anywhere in the repo (including outside the current
      -- directory) must be found, with paths relative to the repo root.
      let files =
            [ (Tracked, "sub/deep/tracked.md", "@(ref:tracked)")
            , (Untracked, "sub/deep/untracked.md", "@(ref:untracked-here)")
            , (Untracked, "root-untracked.md", "@(ref:untracked-root)")
            ]
      withGitRepoAndFileStates files $ do
        let expected =
              SearchResult
                { delims = defaultOpts.delims
                , anchors = mempty
                , references =
                    Map.fromList
                      [ ref "tracked" [loc' "sub/deep/tracked.md" 1 (1, 14)]
                      , ref "untracked-here" [loc' "sub/deep/untracked.md" 1 (1, 21)]
                      , ref "untracked-root" [loc' "root-untracked.md" 1 (1, 21)]
                      ]
                }
        withCurrentDirectory "sub/deep" $
          findRefsFromGit defaultOpts{includeUntracked = True}
            `shouldSatisfy` P.returns (P.eq expected)
