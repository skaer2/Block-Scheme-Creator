module Blocks where

import ASTTypes
import Data.List

type Comment = String

data Block
    = Start (Maybe Comment) Block
    | IOBlock String Block
    | AssignBlock String Block -- Mod
    | Procedure String Block
    | Decision String Block (Maybe Block) Block
    | LoopBlock String Block Block
    | Continue
    | End

removeDefs :: [Action] -> ([Function], [Action])
removeDefs (a1@(Def f):as) = (a1:(fst ), snd next)
removeDefs (a:as) = (fst next, a:(snd next))
    where
        next = removeDefs as

programmToBlock :: Programm -> Block
programmToBlock (Programm c) = Start $ codeToBlock c

codeToBlock :: Code -> Block
codeToBlock (Code c) = actionsToBlock c

actionsToBlock :: [Action] -> Block
actionsToBlock [] = Continue
actionsToBlock (a:as) = actionToBlock

combineSnFnArgs :: Maybe String -> String -> [String] -> String
combineSnFnArgs Nothing = combineFnArgs 
combineSnFnArgs (Just sn) = sn ++ '.':combineFnArgs 

combineFnArgs :: String -> [String] -> String
combineFnArgs fn args = fn ++ '(':combArgs ++ ")"
  where 
    combArgs = (intercalate ", " args)

combineNE :: String -> String -> String
combineNE = (++)

elseToBlock :: Maybe Else -> Maybe Block
elseToBlock Nothing = Nothing
elseToBlock (Just (Else c)) = codeToBlock c

actionToBlock :: Action -> Block -> Block
actionToBlock (Assign (Assignment n e)) = AssignBlock $ combineNE n e
actionToBlock (Call (CallF sn fn args)) = Procedure $ combineSnFnArgs sn fn args
--actionToBlock (Def (Function fn args c)) = Start (combineFnArgs fn args) $ codeToBlock c
actionToBlock (IfBlock (If cond c e)) = Decision cond (codeToBlock c) $ elseToBlock e
actionToBlock (LoopW (While cond c)) = LoopBlock cond codeToLoop
