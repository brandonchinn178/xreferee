{-# LANGUAGE OverloadedStrings #-}

module XReferee.TestUtils (
  defaultOpts,
) where

import XReferee.SearchResult (
  SearchOpts (..),
 )

defaultOpts :: SearchOpts
defaultOpts =
  SearchOpts
    { ignores = []
    }
