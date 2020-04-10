module ASTTypes where

data Code =
    Code [Action]
    deriving (Show, Eq)


type Name = String

type Expr = String

data Action
    = Assign Assignment
    | Call CallF
    | Def Function
    | IfBlock If
    deriving (Show, Eq)

type Condition = String
data If = If Condition Code (Maybe Else)
    deriving (Show, Eq)

data Else = Else Code
    deriving (Show, Eq)

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


