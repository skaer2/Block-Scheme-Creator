module Main where

import PythonParser
import           Control.Applicative ((<|>))
import           Text.ParserCombinators.ReadP
import           Data.Char

main :: IO ()
main = do
  putStrLn "hello world"
