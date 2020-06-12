{----------------------------------------------------------------------------
 - Copyright © 2020 Loren B. Davis.  All rights reserved.                   -
 -                                                                          -
 - This program is hereby released under the three-clause BSD license (see  -
 - LICENSE).                                                                -
 -                                                                          -
 - normal-ha: Perform canonical normalization of UTF-8 input.               -
 ----------------------------------------------------------------------------}

module Main (main) where

import Data.ByteString.Lazy as BL (interact)
import Data.Text.Lazy as TL ( fromChunks, toChunks )
import Data.Text.Lazy.Encoding ( encodeUtf8, decodeUtf8 )
import Data.Text.ICU ( NormalizationMode (NFC), normalize )

main :: IO ()
main = BL.interact (
         encodeUtf8 .
         TL.fromChunks .
         map (normalize NFC) .
         TL.toChunks . -- FIXME: Does this have correct grapheme boundaries?
         decodeUtf8 )
