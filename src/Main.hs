module Main where

import PythonParser
import IndentParsing 
import ASTTypes
import HelperFunctions

import           Control.Applicative ((<|>))
import           Text.ParserCombinators.ReadP
import           Data.Char

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show contents
  putStrLn  "\n\nResults:"
  putStrLn $ show $ showResults $ readP_to_S (codeBlock (-1)) contents
  putStrLn  "\n\nAllOutput:"
  --putStrLn $ show $ showShortOutput $ readP_to_S (codeBlock (-1)) contents
  putStrLn  "\n\nLastOutput:"
  putStrLn $ show $ lastMaybe $ readP_to_S (codeBlock (-1)) contents
  putStrLn  "\n\nResult:"
  putStrLn $ addLineBreaks $ show $ showResult $ readP_to_S (codeBlock (-1)) contents

addLineBreaks :: String -> String
addLineBreaks [] = []
addLineBreaks (x:xs) = if x == ',' then x:'\n':(addLineBreaks xs) else x:(addLineBreaks xs)

lastMaybe :: [a] -> Maybe a
lastMaybe xs = if null xs then Nothing else Just (last xs)
