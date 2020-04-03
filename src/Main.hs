module Main where

import PythonParser
import           Control.Applicative ((<|>))
import           Text.ParserCombinators.ReadP
import           Data.Char

main :: IO ()
main = do
  contents <- getContents
  print contents
  print $ showResult $ readP_to_S (codeParser Nothing) contents
