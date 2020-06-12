{----------------------------------------------------------------------------
 - Copyright © 2020 Loren B. Davis.  All rights reserved.                   -
 -                                                                          -
 - This program is hereby released under the three-clause BSD license (see  -
 - ../LICENSE).                                                             -
 - normalTest: Tests that the data on standard input is UTF-8 in NFC canon- -
 - ical form.                                                               -
 ----------------------------------------------------------------------------}

module Main (main) where

import Data.Text as T (pack)
import Data.Text.IO as TIO ( getContents, putStrLn )
import Data.Text.ICU ( NormalizationMode(NFC), isNormalized )

main :: IO ()
main = do
  contents <- TIO.getContents
  let output = (T.pack . show . isNormalized NFC) contents
  TIO.putStrLn output

