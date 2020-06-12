{----------------------------------------------------------------------------
 - Copyright © 2020 Loren B. Davis.  All rights reserved.                   -
 -                                                                          -
 - This program is hereby released under the three-clause BSD license (see  -
 - ../LICENSE).                                                             -
 - strictTest.hs: A simple wrapper for Data.Text.ICU.normalize.  The prog-  -
 - ram runs only in batch mode, and all data must fit into its memory.      -
 ----------------------------------------------------------------------------}

module Main (main) where

import Data.Text.IO as TIO (interact)
import Data.Text.ICU ( NormalizationMode(NFC), normalize )

main :: IO ()
main = TIO.interact (normalize NFC)

