import Control.Applicative ((<|>))

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
-- Expr = Const | Expr BinOperation Expr | UnOperation Expr
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

data Expr = Const Int | BinOperation Expr Expr | UnOperation Expr

