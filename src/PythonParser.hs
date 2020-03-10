module PythonParser where

import           Control.Applicative          ((<|>))
import           Data.Char
import           Text.ParserCombinators.ReadP

-- Code = List Actions
--
-- Actions = Assignment | Function | Call | Keywords
--
-- Assignment = Name Assign Expr
--              | Name BinOperation Assign Expr
--
-- Assign = '='
-- Name = String starting with a letter
--
-- Expr = String
--
-- Function = Def Name '(' Arguments ')' ':'
--            BlockBegin Code BlockEnd
-- BlockBegin = IndentIn
-- BlockEnd   = IndentOut
--
data Code =
    Code [Action]
    deriving (Show, Eq)

data Action 
    = Assign Assignment
    | Call CallF 
    | Definition Function
--  | Use Keyword
    deriving (Show, Eq)

data Assignment =
    Assignment Name Expr
    deriving (Show, Eq)

type Name = String

type Expr = String

data CallF = CallF (Maybe SourceName) FunctionName [Argument]
    deriving (Show, Eq)

type SourceName = String

type FunctionName = String

type Argument = String

data Function = Function FunctionName [Argument]

showResult :: [(a, String)] -> Maybe a
showResult r = case showResults r of
                 [] -> Nothing
                 (x:_) -> Just $ fst x

showResults :: [(a, String)] -> [(a, String)]
showResults =
    filter
        (\(x, leftovers) ->
             if leftovers == ""
                 then True
                 else False)

letter :: ReadP Char
letter = satisfy isAlpha

digit :: ReadP Char
digit = satisfy isDigit

number :: ReadP Int
number = fmap (read) $ many1 digit

isEndOfLine :: Char -> Bool
isEndOfLine = (==) '\n'

stringParser :: ReadP String
stringParser = many1 $ satisfy $ not . isEndOfLine

nameParser :: ReadP String
nameParser = do
    firstLetter <- letter
    everythingElse <- many $ satisfy isAlphaNum
    return $ firstLetter : everythingElse

endOfLine :: ReadP ()
endOfLine = (satisfy isEndOfLine >> return ())<|> eof 

codeParser :: ReadP Code
codeParser = fmap Code $ many1 actionParser

actionParser :: ReadP Action
actionParser = (fmap Assign assigmentParser) <|> (fmap Call callParser)

assigmentParser :: ReadP Assignment
assigmentParser = do
    name <- nameParser 
    skipSpaces
    char '='
    skipSpaces
    expr <- stringParser 
    endOfLine
    return $ Assignment name expr

callParser :: ReadP CallF
callParser = do 
    sourceName <- option Nothing source
    functionName <- nameParser 
    char '('
    arguments <- sepBy stringParser (skipSpaces >> char ',' >> skipSpaces)
    char ')'
    endOfLine
    return $ CallF sourceName functionName arguments
  where
    source = do 
        name <- nameParser
        char '.'
        return $ Just name

functionParser :: ReadP Function
