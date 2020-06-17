{----------------------------------------------------------------------------
 - Copyright © 2020 Loren B. Davis.  All rights reserved.                   -
 -                                                                          -
 - This program is hereby released under the three-clause BSD license (see  -
 - LICENSE).                                                                -
 -                                                                          -
 - normal-ha: Perform canonical normalization of UTF-8 input.               -
 ----------------------------------------------------------------------------}

module Main ( lazyNormalize, main ) where

import Data.ByteString as B (ByteString)
{- A previous design generated the output as a Builder.  However, it consisted
 - only of strict ByteStrings inserted with a hard break.  I therefore simplify
 - it to generate a list of strict ByteStrings instead.
 -}
-- import Data.ByteString.Builder ( Builder, toLazyByteString )
-- import Data.ByteString.Builder.Extra (byteStringInsert)
import Data.ByteString.Lazy as BL ( ByteString, fromChunks, interact )
import Data.Text as T ( Text, append, empty, last )
import Data.Text.Lazy as TL (toChunks)
import Data.Text.Lazy.Encoding as LE (decodeUtf8)
import Data.Text.Encoding as E (encodeUtf8)
import Data.Text.ICU ( NormalizationMode (NFC), LocaleName(Current),
  breakCharacter, breaksRight, brkBreak, brkPrefix, brkSuffix, normalize )
import Data.Text.ICU.Char ( GeneralCategory_ (GeneralCategory),
  GeneralCategory( ControlChar, LineSeparator, ParagraphSeparator ), property )

main :: IO ()
main = BL.interact (lazyNormalize NFC)

lazyNormalize :: NormalizationMode -> BL.ByteString -> BL.ByteString
{- A wrapper to normalize a lazy UTF-8 ByteString with Data.Text.ICU.normalize.
 -}
lazyNormalize mode = BL.fromChunks .
                     Prelude.map (normalizeChunk mode) .
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
 - With one exception, the final grapheme of the chunk is split off and passed
 - to the next (tail-recursive modulo cons) call to be prepended to the follow-
 - ing chunk.  That one exception is when the chunk ends with a Unicode control
 - character, line separator or paragraph separator.  The next chunk could start
 - with a combining character that could in general change the canonical form of
 - the preceding grapheme, but not if it is a character from one of these
 - classes.  The motivation for adding code to handle this special case is so
 - that, in interactive mode, the program will echo newlines.  It is more effic-
 - ient as well.
 -}
        go left [] | left == T.empty = []
        go left []                   = [left]
        go left (h:t) | h == T.empty = go left t
                      | doNotSplit   = chunk:(go T.empty t)
                      | Prelude.length graphemesInReverse < 2
                                     = go chunk t
                      | brkSuffix ultimo /= T.empty =
          error "Chunk contained something after its final break."
                      | otherwise    = h':(go residue t)
          where h' = T.append left right -- Should this be evaluated strictly?
                chunk = T.append left h  -- Should this be evaluated strictly?
                doNotSplit = case (property GeneralCategory . T.last) h of
                                  ControlChar        -> True
                                  LineSeparator      -> True
                                  ParagraphSeparator -> True
                                  _                  -> False
                graphemeBreaker = breakCharacter Current
                graphemesInReverse = breaksRight graphemeBreaker h
                ultimo = head graphemesInReverse
                right = brkPrefix ultimo
                residue = brkBreak ultimo

normalizeChunk :: NormalizationMode -> T.Text -> B.ByteString
{- Converts a strict chunk of Text to NFC normalized, UTF8-encoded form. Since
 - each chunk was being flushed immediately anyway, this now returns a strict
 - ByteString which will be combined into a list, then a lazy ByteString.)
 -}
normalizeChunk mode = E.encodeUtf8 .
                      normalize mode

