{-# LANGUAGE OverloadedStrings #-}

module XReferee.TestUtils (
  defaultOpts,
  anchor,
  ref,
  loc,
) where

import Data.Text (Text)
import XReferee.SearchResult (
  Anchor (..),
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
loc = LabelLoc
