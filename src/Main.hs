module Main where

import           ASTTypes
import           HelperFunctions
import           IndentParsing
import           PythonParser

import           Control.Applicative          ((<|>))
import           Data.Char
import           Text.ParserCombinators.ReadP

main :: IO ()
main = do
    contents <- getContents
    putStrLn $ show contents
    putStrLn "\n\nResults:"
    putStrLn $ show $ showResults $ readP_to_S (codeBlock (-1)) contents
    putStrLn "\n\nAllOutput:"
    --putStrLn $ show $ showShortOutput $ readP_to_S (codeBlock (-1)) contents
    putStrLn "\n\nResult:"
    putStrLn $ addLineBreaks $ show $ showResult $ readP_to_S (codeBlock (-1)) contents
    putStrLn "\n\n"
    putStrLn "\n\n"
    putStrLn "parse Python:\n"
    let result = parsePython contents 
    case result of
        Left s -> putStrLn "Error\nUnread string:\n" >> putStrLn s
        Right p -> putStrLn $ addLineBreaks $ show p

