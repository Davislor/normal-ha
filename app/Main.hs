{----------------------------------------------------------------------------
 - Copyright © 2020 Loren B. Davis.  All rights reserved.                   -
 -                                                                          -
 - This program is hereby released under the three-clause BSD license (see  -
 - LICENSE).                                                                -
 -                                                                          -
 - normal-ha: Perform canonical normalization of UTF-8 input.               -
 ----------------------------------------------------------------------------}

module Main (main) where

import Data.ByteString.Lazy as BL
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding ( encodeUtf8, decodeUtf8 )
import Data.Text.ICU ( NormalizationMode (..), normalize, fromUtf8 )

main :: IO ()
main = BL.interact (
         encodeUtf8 .
         TL.fromStrict .
         normalize NFC .
         TL.toStrict .
         decodeUtf8 )
