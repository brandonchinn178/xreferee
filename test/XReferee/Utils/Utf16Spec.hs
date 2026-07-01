{-# LANGUAGE OverloadedStrings #-}

module XReferee.Utils.Utf16Spec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE
import Skeletest
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range
import XReferee.Utils.Utf16 (utf16Length)

spec :: Spec
spec = do
  describe "utf16Length" $ do
    prop "matches reference implementation via encodeUtf16BE" $ do
      t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicodeAll
      let bs = LBS.fromStrict (TE.encodeUtf8 t)
      -- 1 UTF-16 code unit = 2 bytes, so we divide the length of the encoded
      -- bytes by 2 to get the expected number of UTF-16 code units.
      let expected = BS.length (TE.encodeUtf16BE t) `div` 2
      utf16Length bs `shouldBe` expected
