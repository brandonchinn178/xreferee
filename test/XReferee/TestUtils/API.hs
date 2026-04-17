{-# LANGUAGE OverloadedStrings #-}

module XReferee.TestUtils.API (
  defaultOpts,
  anchor,
  ref,
  loc,
  loc',
) where

import Data.Text (Text)
import XReferee.SearchResult (
  Anchor (..),
  ColumnRange (..),
  LabelLoc (..),
  Reference (..),
  SearchOpts (..),
 )

defaultOpts :: SearchOpts
defaultOpts =
  SearchOpts
    { ignores = []
    }

anchor :: Text -> [LabelLoc] -> (Anchor, [LabelLoc])
anchor name locs = (Anchor name, locs)

ref :: Text -> [LabelLoc] -> (Reference, [LabelLoc])
ref name locs = (Reference name, locs)

loc :: FilePath -> Int -> LabelLoc
loc filepath lineNum =
  loc' filepath lineNum dummyColumnRange
  where
    dummyColumnRange = (0, 0)

loc' :: FilePath -> Int -> (Int, Int) -> LabelLoc
loc' filepath lineNum (startCol, endCol) =
  LabelLoc
    { filepath
    , lineNum
    , columnRange = ColumnRange{start = startCol, end = endCol}
    }
