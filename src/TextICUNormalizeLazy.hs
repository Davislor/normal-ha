module TextICUNormalizeLazy (lazyNormalize)
  where

import Data.Text as T ( Text, append, empty, last )
import Data.Text.Lazy as TL ( Text, fromChunks, toChunks )
import Data.Text.ICU ( NormalizationMode (NFC), LocaleName(Current),
  breakCharacter, breaksRight, brkBreak, brkPrefix, brkSuffix, normalize )
import Data.Text.ICU.Char ( GeneralCategory_ (GeneralCategory),
  GeneralCategory( ControlChar, LineSeparator, ParagraphSeparator ), property )

lazyNormalize :: NormalizationMode -> TL.Text -> TL.Text
{- A wrapper to normalize a lazy Text with Data.Text.ICU.normalize.
 -}
lazyNormalize mode = TL.fromChunks .
                     Prelude.map ( normalize mode ) .
                     go T.empty .
                     TL.toChunks
  where go :: T.Text -> [T.Text] -> [T.Text]
{- Breaks the lazy Text into a list of strict Texts, each of which ends on a
 - grapheme boundary, and whose concatenation is the same as the original input.
 -
 - It cannot perform look-ahead on the next chunk, as the program would then
 - hang waiting in interactive mode.  It instead checks whether the chunk ends
 - on a line boundary or end of file.
 -
 - If so, the chunk is processed in its entirety.  This means that full lines
 - including newlines are processed immediately in interactive mode.  Otherwise,
 - the first character of the next chunk might be a combining character that
 - changes the canonical normalization of the last grapheme of this chunk.  It
 - is split off and passed to the (tail-recursive modulo cons) next invocation
 - to be prepended to the following chunk.
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
