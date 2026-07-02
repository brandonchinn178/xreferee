{-# LANGUAGE BinaryLiterals #-}

module XReferee.Utils.Utf16 (
  utf16Length,
) where

import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LBS

-- | Takes a UTF-8-encoded bytestring and counts the number of UTF-16 code units.
utf16Length :: LazyByteString -> Int
utf16Length = LBS.foldl' (\n w -> n + codeUnits w) 0
  where
    -- In UTF-16, codepoints from the Basic Multilingual Plane (BMP) are encoded as a single code unit,
    -- while codepoints outside the BMP are encoded as a surrogate pair, which counts as two code units.
    --
    -- This function goes through a UTF-8 encoded bytestring, see: https://en.wikipedia.org/wiki/UTF-8#Description
    -- If it finds a codepoint from the BMP, it adds 1 to the counter.
    -- If it finds a codepoint outside the BMP, it adds 2 to the counter.
    codeUnits w
      -- Single byte codepoint
      | w < 0b10000000 = 1
      -- Continuation byte
      | w < 0b11000000 = 0
      -- A byte marking the sequence of a 2/3 byte codepoint, up to U+FFFF.
      -- Such codepoints belong to the BMP, so they correspond to 1x UTF-16 code unit.
      | w < 0b11110000 = 1
      -- A byte marking the sequence of a 4-byte codepoint, U+10000 and above.
      -- Such codepoints are outside the BMP and are encoded as a surrogate pair, counting as 2x UTF-16 code units.
      | otherwise = 2
