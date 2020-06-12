{----------------------------------------------------------------------------
 - Copyright © 2020 Loren B. Davis.  All rights reserved.                   -
 -                                                                          -
 - This program is hereby released under the three-clause BSD license (see  -
 - LICENSE).                                                                -
 -                                                                          -
 - normal-ha: Perform canonical normalization of UTF-8 imput.               -
 ----------------------------------------------------------------------------}

module Main (main) where

import Data.ByteString.Lazy as BL
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as EL
import Data.Text.ICU.Normalize (NormalizationMode, normalize)

main :: IO ()
main = BL.interact (
         EL.encodeUtf8 .
         id .
         EL.decodeUtf8 )
