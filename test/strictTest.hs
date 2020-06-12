module Main (main) where

import Data.Text.IO as TIO (interact)
import Data.Text.ICU ( NormalizationMode(NFC), normalize )

main :: IO ()
main = TIO.interact (normalize NFC)

