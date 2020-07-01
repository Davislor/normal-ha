{----------------------------------------------------------------------------
 - Copyright © 2020 Loren B. Davis.  All rights reserved.                   -
 -                                                                          -
 - This program is hereby released under the three-clause BSD license (see  -
 - LICENSE).                                                                -
 -                                                                          -
 - normal-ha: Perform canonical normalization of UTF-8 input.               -
 ----------------------------------------------------------------------------}

module Main ( lazyNormalize, main ) where

import Data.ByteString.Lazy as BL ( fromChunks, interact )
import Data.Text.Encoding as E (encodeUtf8)
import Data.Text.Lazy as TL (toChunks)
import Data.Text.Lazy.Encoding as LE (decodeUtf8)
import Data.Text.ICU ( NormalizationMode (NFC) )
import TextLazyICUNormalize (lazyNormalize)

main :: IO ()
main = BL.interact (
         BL.fromChunks .
         map E.encodeUtf8 .
         TL.toChunks . -- Workaround for buffer not always flushing on newline.
         lazyNormalize NFC .
         LE.decodeUtf8 )



