{----------------------------------------------------------------------------
 - Copyright © 2020 Loren B. Davis.  All rights reserved.                   -
 -                                                                          -
 - This program is hereby released under the three-clause BSD license (see  -
 - LICENSE).                                                                -
 -                                                                          -
 - normal-ha: Perform canonical normalization of UTF-8 input.               -
 ----------------------------------------------------------------------------}

module Main (main) where

import Data.ByteString.Builder ( Builder, toLazyByteString )
import Data.ByteString.Builder.Extra (byteStringInsert)
import Data.ByteString.Lazy as BL ( ByteString, interact )
import Data.Text as T -- (Text, empty)
import Data.Text.Lazy as TL -- (Text, empty)
import Data.Text.Lazy.Encoding as LE (decodeUtf8)
import Data.Text.Encoding as E (encodeUtf8)
import Data.Text.ICU -- ( NormalizationMode (NFC), normalize )

main :: IO ()
main = BL.interact (
         toLazyByteString .
         foldMap normalizeChunk .
         decode )

decode :: BL.ByteString -> [T.Text]
{- Breaks the lazy ByteString into a list of strict ByteStrings, each of which
 - ends on a grapheme boundary, and whose concatenation is the same as the
 - original input.
 -
 - This uses the pure Breaker API, finding the last grapheme break in each
 - chunk and then copying the leftovers from the previous chunk, plus every-
 - thing up to the last grapheme boundary in this chunk, to a new chunk. This
 - makes more deep copies than necessary.
 -}
decode = go T.empty .
         toChunks .
         LE.decodeUtf8
  where go :: T.Text -> [T.Text] -> [T.Text]
        go left [] | left == T.empty = []
        go left []                   = [left]
        go left (h:t)                  =
          ((TL.toStrict . TL.fromChunks) [left, middle, right]):
          (go residue t)
          where graphemeBreaker = breakCharacter Current
                lastGrapheme = Prelude.head (breaksRight graphemeBreaker h)
                middle = brkPrefix lastGrapheme
                right = brkBreak lastGrapheme
                residue = brkSuffix lastGrapheme

normalizeChunk :: T.Text -> Builder
normalizeChunk = byteStringInsert .
                 E.encodeUtf8 .
                 normalize NFC

