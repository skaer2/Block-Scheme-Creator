module Main where

import           ASTTypes
import           Blocks
import           FTests
import           HelperFunctions
import           IndentParsing
import           PythonParser

import           DrawBlocks
import           Diagrams.Backend.SVG.CmdLine

import           Control.Applicative          ((<|>))
import           Data.Char
import           Text.ParserCombinators.ReadP

main :: IO ()
--main = drawBlocks
main = mainWith main'

main' :: FilePath -> IO (Diagram B)
main' file = do
    contents <- readFile file
    --putStrLn $ show contents
    --putStrLn "\n\nResults:"
    --putStrLn $ show $ showResults $ readP_to_S (codeBlock (-1)) contents
    --putStrLn "\n\nAllOutput:"
    --putStrLn $ show $ showShortOutput $ readP_to_S (codeBlock (-1)) contents
    --putStrLn "\n\nResult:"
    --putStrLn $ addLineBreaks $ show $ showResult $ readP_to_S (codeBlock (-1)) contents
    --putStrLn "\n\n"
    --putStrLn "\n\n"
    --putStrLn "parse Python:\n"
    let result = parsePython contents
    case result of
        Left s -> putStrLn "Error\nUnread string:\n" >> putStrLn s >> return (mempty)
        Right p@(Programm (Code as)) -> do
            --putStrLn "Right:"
            --putStrLn $ addLineBreaks $ show p
            --c <- getChar
            --if c == 'n'
                --then return (mempty)
                --else putStrLn "\nf1:\n" >> f1 as
            --c2 <- getChar
            --if c2 == 'n'
                --then return (mempty)
                --else putStrLn "\nf2:\n" >> f2 as
            --c3 <- getChar
            --if c3 == 'n'
                --then return (mempty)
                --else putStrLn "\nBlocks:\n" >> f3 p
            return $ blocksToDiagram $ map programmToBlock $ readyProgramm p --Vremeno


f3 p = putStrLn $ show $ map programmToBlock $ readyProgramm p
