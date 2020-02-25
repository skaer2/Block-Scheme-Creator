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

data Operation = Add | Minus

letter :: ReadP Char
letter :: = satisfy isAlpha

digit :: ReadP Char
digit = satisfy isDigit

number :: ReadP Int
number = many1 digit

exprParser :: ReadP Expr
exprParser = (fmap Variable nameParser) <|> constParser <|> binOperationParser 

constParser :: ReadP Expr
constParser = number

nameParser :: ReadP String
nameParser = do
    firstLetter <- letter 
    everythingElse <- many $ satisfy isAlphaNum
    return $ firstLetter:everythingElse
    
operatorParser :: ReadP Operation 
operatorParser = choice operatorParsers
    where 
        operatorParsers = [addParser, minusParser]

addParser :: ReadP Operation
addParser = do 
    char '+'
    return Add 

minusParser :: ReadP Operation
minusParser = do
    char '-'
    return Minus

binOperation :: ReadP Expr
binOperation = do
    char '('
    skipSpaces
    leftOperand <- exprParser
    operator <- operatorParser
    rightOperand <- exprParser
    skipSpaces
    char ')'
    return $ BinOperation operator leftOperand rightOperand
