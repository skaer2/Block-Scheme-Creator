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
data Code =
    Code [Actions]
    deriving (Show, Eq)

data Actions =
    Action Assignment
    deriving (Show, Eq)

data Assignment =
    Assignment Name Expr
    deriving (Show, Eq)

type Name = String

data Expr =
    Expr Term [(Operator, Term)]
    deriving (Show, Eq)

data Term =
    Term Factor [(Operator, Factor)]
    deriving (Show, Eq)

data Factor
    = Variable String
    | Const Int
    | FactorExpr Expr
    deriving (Show, Eq)

data Operator
    = Add
    | Minus
    | Product
    deriving (Show, Eq)

evaluateExpr :: Expr -> Int
evaluateExpr (Expr term terms) =
    foldl f (evaluateTerm term) terms
  where
      f acc (op, termR) = (toFunction op) acc (evaluateTerm termR) 

evaluateTerm :: Term -> Int
evaluateTerm (Term factor factors) =
    foldl f (evaluateFactor factor) factors
  where
      f acc (op, factorR) = (toFunction op) acc (evaluateFactor factorR) 

evaluateFactor :: Factor -> Int
evaluateFactor (Variable s)      = 0
evaluateFactor (Const n)         = n
evaluateFactor (FactorExpr expr) = evaluateExpr expr

toFunction :: (Num a) => Operator -> (a -> a -> a)
toFunction Add     = (+)
toFunction Minus   = (-)
toFunction Product = (*)

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

exprParser :: ReadP Expr
exprParser = do
    leftTerm <- termParser
    terms <- many sequenceOfTerms
    return $ Expr leftTerm terms
  where
    sequenceOfTerms = do
        operator <- minusParser <|> addParser
        rightTerm <- termParser
        return $ (operator, rightTerm)

termParser :: ReadP Term
termParser = do
    leftFactor <- factorParser
    factors <- many sequenceOfFactors
    return $ Term leftFactor factors
  where
    sequenceOfFactors = do
        operator <- productParser
        rightFactor <- factorParser
        return $ (operator, rightFactor)

factorParser :: ReadP Factor
factorParser =
    constParser <|> do
        char '('
        expr <- (fmap FactorExpr exprParser) {--(fmap Variable nameParser) <|>--}
        char ')'
        return expr

constParser :: ReadP Factor
constParser = fmap Const number

nameParser :: ReadP String
nameParser = do
    firstLetter <- letter
    everythingElse <- many $ satisfy isAlphaNum
    return $ firstLetter : everythingElse

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
        {--
binOperationParser :: ReadP Expr
binOperationParser = do
    char '('
    leftOperand <- exprParser
    ):_ <- many1 inBetween
    char ')'
    return $ BinOperation operator leftOperand rightOperand
  where
    inBetween = do
        operator <- operatorParser
        rightOperand <- exprParser
        return (operator, rightOperand)

unOperationParser :: ReadP Expr
unOperationParser = do
    char '('
    operand <- exprParser
    char ')'
    return $ UnOperation operand
    --}
