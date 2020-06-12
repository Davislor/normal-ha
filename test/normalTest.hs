module Main (main) where

import Data.Text as T (pack)
import Data.Text.IO as TIO ( getContents, putStrLn )
import Data.Text.ICU ( NormalizationMode(NFC), isNormalized )

main :: IO ()
main = do
  contents <- TIO.getContents
  let output = (T.pack . show . isNormalized NFC) contents
  TIO.putStrLn output

