module Main where

import           ASTTypes
import           FTests
import           HelperFunctions
import           IndentParsing
import           PythonParser

import           Control.Applicative          ((<|>))
import           Data.Char
import           Text.ParserCombinators.ReadP

main :: IO ()
main = do
    contents <- readFile "pythonexamplecode.txt"
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
        Right p@(Programm (Code as)) -> do
            putStrLn $ addLineBreaks $ show p
            c <- getChar
            if c == 'n'
                then return ()
                else putStrLn "\n\n" >> f1 as

            c2 <- getChar
            if c2 == 'n'
                then return ()
                else putStrLn "\n\n" >> f2 as
