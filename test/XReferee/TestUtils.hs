{-# LANGUAGE OverloadedStrings #-}

module XReferee.TestUtils (
  anchor,
  ref,
  defaultOpts,
) where

import Data.Text (Text)
import XReferee.SearchResult (
  Label (..),
  SearchOpts (..),
 )

defaultOpts :: SearchOpts
defaultOpts =
  SearchOpts
    { ignores = []
    }

anchor :: Text -> Label
anchor name = Label name ("@@(" <> name <> ")")

ref :: Text -> Label
ref name = Label name ("^^(" <> name <> ")")
