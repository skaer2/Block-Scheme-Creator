module PythonParser where

import           Control.Applicative ((<|>))
import           Text.ParserCombinators.ReadP
import           Data.Char

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
-- Expr = Name | Const | Expr BinOperation Expr | UnOperation Expr
--
-- BinOperation = Add | Substract 
--          | Mult | Divide 
--          | Div | Mod 
--          | Power 
--          | Equal | More | Less 
--          | MoreOrEqual | LessOrEqual 
--          | NotEqual 
--          | Or | And
--
-- Add = '+'
-- Substract = '-'
--
-- UnOperation = Not          --Also Operators on bits
--
--
-- Function = Def Name '(' Arguments ')' ':' 
--            BlockBegin Code BlockEnd
-- BlockBegin = IndentIn
-- BlockEnd   = IndentOut
--

data Code = Code [Actions]
    deriving (Show, Eq)

data Actions = Action Assignment
    deriving (Show, Eq)

data Assignment = Assignment Name Expr
    deriving (Show, Eq)

type Name = String

data Expr = Variable String | Const Int 
          | BinOperation Operator Expr Expr | UnOperation Expr
    deriving (Show, Eq)

data Operator = Add | Minus | Product
    deriving (Show, Eq)

showResult :: [(a, String)] -> [(a, String)]
showResult = filter (\(x, leftovers) -> if leftovers == "" then True else False)

letter :: ReadP Char
letter = satisfy isAlpha

digit :: ReadP Char
digit = satisfy isDigit

number :: ReadP Int
number = fmap (read) $ many1 digit

exprParser :: ReadP Expr
exprParser = {--(fmap Variable nameParser) <|>--} constParser <|> binOperationParser 

constParser :: ReadP Expr
constParser = fmap Const number

nameParser :: ReadP String
nameParser = do
    firstLetter <- letter 
    everythingElse <- many $ satisfy isAlphaNum
    return $ firstLetter:everythingElse
    
operatorParser :: ReadP Operator 
operatorParser = choice operatorParsers
    where 
        operatorParsers = [addParser, minusParser, productParser]

addParser :: ReadP Operator
addParser = do 
    char '+'
    return Add 

minusParser :: ReadP Operator
minusParser = do
    char '-'
    return Minus

productParser :: ReadP Operator
productParser = do
    char '*'
    return Product

binOperationParser :: ReadP Expr
binOperationParser = do
    --char '('
    --skipSpaces
    leftOperand <- constParser
    --skipSpaces
    operator <- operatorParser
    --skipSpaces
    rightOperand <- exprParser
    --skipSpaces
    --char ')'
    return $ BinOperation operator leftOperand rightOperand
