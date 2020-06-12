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
import Data.ByteString.Lazy as BL ( ByteString, interact )
import Data.Text as T (Text)
import Data.Text.Lazy as TL (toChunks)
import Data.Text.Lazy.Encoding as LE (decodeUtf8)
import Data.Text.Encoding as E (encodeUtf8Builder)
import Data.Text.ICU ( NormalizationMode (NFC), normalize )

main :: IO ()
main = BL.interact (
         toLazyByteString .
         mconcat .
         Prelude.map normalizeChunk .
         decode )

decode :: BL.ByteString -> [T.Text]
decode = TL.toChunks . -- FIXME: Break on grapheme boundaries instead?
         LE.decodeUtf8
         
normalizeChunk :: T.Text -> Builder
normalizeChunk = encodeUtf8Builder .
                 normalize NFC

