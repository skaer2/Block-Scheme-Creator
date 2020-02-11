import Control.Applicative ((<|>))

-- Code = List Actions
-- Actions = Assignment | Function | Call | Keywords
-- Assignment = Name Assign Expr
-- Expr = Const | Expr BinOperation Expr | UnOperation Expr
-- BinOperation = Add | Substract 
--          | Mult | Divide 
--          | Div | Mod 
--          | Power 
--          | Equal | More | Less 
--          | MoreOrEqual | LessOrEqual 
--          | NotEqual 
-- UnOperation = 
--
