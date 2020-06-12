{----------------------------------------------------------------------------
 - Copyright © 2020 Loren B. Davis.  All rights reserved.                   -
 -                                                                          -
 - This program is hereby released under the three-clause BSD license (see  -
 - LICENSE).                                                                -
 -                                                                          -
 - normal-ha: Perform canonical normalization of UTF-8 input.               -
 ----------------------------------------------------------------------------}

module Main ( lazyNormalize, main ) where

import Data.ByteString.Builder ( Builder, toLazyByteString )
import Data.ByteString.Builder.Extra (byteStringInsert)
import Data.ByteString.Lazy as BL ( ByteString, interact )
import Data.Text as T ( Text, append, empty )
import Data.Text.Lazy as TL ( fromChunks, toChunks, toStrict )
import Data.Text.Lazy.Encoding as LE (decodeUtf8)
import Data.Text.Encoding as E (encodeUtf8)
import Data.Text.ICU ( NormalizationMode (NFC), LocaleName(Current),
  breakCharacter, breaksRight, brkBreak, brkPrefix, brkSuffix, normalize )

main :: IO ()
main = BL.interact (lazyNormalize NFC)

lazyNormalize :: NormalizationMode -> BL.ByteString -> BL.ByteString
{- A wrapper to normalize a lazy UTF-8 ByteString with Data.Text.ICU.normalize.
 -}
lazyNormalize mode = toLazyByteString .
                     foldMap (normalizeChunk mode) .
                     go T.empty .
                     TL.toChunks .
                     LE.decodeUtf8
  where go :: T.Text -> [T.Text] -> [T.Text]
 {- Breaks the lazy ByteString into a list of strict ByteStrings, each of which
 - ends on a grapheme boundary, and whose concatenation is the same as the
 - original input.
 -
 - This uses the pure Breaker API, finding the last grapheme break in each
 - chunk and then copying the leftovers from the previous chunk, plus every-
 - thing up to the last grapheme boundary in this chunk, to a new chunk. This
 - makes more deep copies than necessary, but will produce chunks that always
 - break on a grapheme boundary.  These will be short if typing in interactive
 - mode or long if running a batch job.
 -
 - The final grapheme in any chunk is always passed on to the next tail-
 - recursive (modulo cons) call, as the first character of the next chunk might
 - be a combining character that changes its canonical normalization.
 -}
        go left [] | left == T.empty = []
        go left []                   = [left]
        go left (h:t) | length graphemesInReverse < 2
                                     = go (T.append left h) t
                      | otherwise    = h':go residue t
          where h' = ((TL.toStrict . TL.fromChunks) [left, middle, right])
                graphemeBreaker = breakCharacter Current
                graphemesInReverse = breaksRight graphemeBreaker h
                penult = (head . tail) graphemesInReverse
                middle = brkPrefix penult
                right = brkBreak penult
                residue = brkSuffix penult

normalizeChunk :: NormalizationMode -> T.Text -> Builder
{- Converts a strict chunk of Text to NFC normalized, UTF8-encoded form, then
 - returns it as a Builder.  Each chunk will be flushed immediately, for the
 - sake of interactive operation.  (But, if running on batch data, the chunks
 - should be adequately large.)
 -}
normalizeChunk mode = byteStringInsert .
                      E.encodeUtf8 .
                      normalize mode

