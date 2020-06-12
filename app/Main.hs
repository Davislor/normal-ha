{----------------------------------------------------------------------------
 - Copyright © 2020 Loren B. Davis.  All rights reserved.                   -
 -                                                                          -
 - This program is hereby released under the three-clause BSD license (see  -
 - LICENSE).                                                                -
 -                                                                          -
 - normal-ha: Perform canonical normalization of UTF-8 imput.               -
 ----------------------------------------------------------------------------}

module Main (main) where

import Data.ByteString as B
import Data.Text.Encoding as E
import Data.Text.ICU ( NormalizationMode (..), normalize, fromUtf8 )

main :: IO ()
main = B.interact (
         E.encodeUtf8 .
         normalize NFC .
         E.decodeUtf8 )
