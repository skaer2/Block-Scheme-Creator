module ASTTypes where

data Code =
    Code [Action]
    deriving (Show, Eq)


type Name = String

type Expr = String

data Action
    = Assign Assignment
    | Call CallF
    | Definition Function
    deriving (Show, Eq)

--  | Use Keyword
data Assignment =
    Assignment Name Expr
    deriving (Show, Eq)


type SourceName = String

type FunctionName = String

type Argument = String

data CallF =
    CallF (Maybe SourceName) FunctionName [Argument]
    deriving (Show, Eq)

data Function =
    Function FunctionName [Argument] Code
    deriving (Show, Eq)


