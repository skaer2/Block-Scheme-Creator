module Blocks where

import           ASTTypes
import           Data.List

type Comment = String

data Block
    = Start (Maybe Comment) Block
    | IOBlock String Block
    | AssignBlock String Block -- Mod
    | Procedure String Block
    | Decision String Block (Maybe Block) Block
    | LoopBlock String Block 
    | LoopEnd String Block
    | Next
    | End (Maybe String)
    deriving (Show)

--actionToBlock (Def (Function fn args c)) names =
    --Start (Just $ combineFnArgs fn args) $ codeToBlock c names

defaultNames = map (\c -> c:[]) ['A'..'Z']

isIOAction :: String -> Bool
isIOAction s = s `elem` ioActions
    where
        ioActions = ["print", "input"]

isExitAction :: String -> Bool
isExitAction s = s `elem` exitActions
    where
        exitActions = ["exit"]

functionsToBlock :: [Action] -> [Block]
functionsToBlock as = map (\a -> actionsToBlock [a] (End Nothing) defaultNames) as

programmToBlock :: Programm -> Block
programmToBlock (Programm c) = Start Nothing $ codeToBlock c (End Nothing) defaultNames

codeToBlock :: Code -> Block -> [String] -> Block
codeToBlock (Code as) = actionsToBlock as

actionsToBlock :: [Action] -> Block -> [String] -> Block
actionsToBlock [] lastB _     = lastB
actionsToBlock (a:as) lastB names = actionToBlock a names $ actionsToBlock as lastB names

combineSnFnArgs :: Maybe String -> String -> [String] -> String
combineSnFnArgs Nothing fn args = combineFnArgs fn args
combineSnFnArgs (Just sn) fn args = sn ++ '.' : (combineFnArgs fn args)

combineFnArgs :: String -> [String] -> String
combineFnArgs fn args = fn ++ ('(' : combArgs ++ ")")
  where
    combArgs = (intercalate ", " args)

combineNE :: String -> String -> String
combineNE s1 s2 = s1 ++ " = " ++ s2 

elseToBlock :: Maybe Else -> [String] -> Maybe Block
elseToBlock Nothing _             = Nothing
elseToBlock (Just (Else c)) names = Just (codeToBlock c Next names)

codeToLoop :: Code -> String -> [String] -> Block -> Block
codeToLoop c s names b = codeToBlock c (LoopEnd s b) names

flowActToBlock :: FlowAct -> Block -> Block
flowActToBlock Break _      = Next
flowActToBlock Continue _   = Next
flowActToBlock (Return s) _ = End (Just s)
flowActToBlock (Yield s) _  = End (Just s)

actionToBlock :: Action -> [String] -> Block -> Block
actionToBlock (Assign (Assignment n e)) _ = AssignBlock $ combineNE n e
actionToBlock (Call (CallF sn fn args)) _ =
    if isIOAction fn
        then IOBlock s
        else if isExitAction fn
                 then (\_ -> End Nothing)
                 else Procedure s
  where
    s = combineSnFnArgs sn fn args
actionToBlock (Def (Function fn args c)) names = \_ -> Start (Just $ combineFnArgs fn args) (codeToBlock c (End Nothing) defaultNames)
actionToBlock (IfBlock (If cond c e)) names =
    Decision cond (codeToBlock c Next names) $ elseToBlock e names
actionToBlock (LoopW (While cond c)) (name:names) =
    LoopBlock (name ++ " While " ++ cond) . codeToLoop c name names
actionToBlock (LoopF (For var cond c)) (name:names) =
    LoopBlock (name ++ " For " ++ var ++ " in " ++ cond) . codeToLoop c name names
actionToBlock (Flow flAct) _ = flowActToBlock flAct

theblock2 = [Start Nothing (LoopBlock "A For x in range(5)" (IOBlock "print(\"Hello World\\n\")" (LoopEnd "A" (AssignBlock "round1 = int(raw_input(Enter score for round 1: ))" (LoopBlock "A While n != guess" (Decision "guess < n" (IOBlock "print(guess)" (AssignBlock "guess = int(raw_input())" Next)) (Just (IOBlock "print(you)" Next)) (LoopEnd "A" (End Nothing)))))))),Start Nothing (AssignBlock "define function a3" (End Nothing)),Start Nothing (AssignBlock "define function raw_input" (End Nothing))]

theblock1 = [Start Nothing (LoopBlock "A For x in range(5)" (IOBlock "print(\"Hello World\\n\")" (LoopEnd "A" (End Nothing))))]

theblock = [Start Nothing (AssignBlock "round1 = int(raw_input(Enter score for round 1: ))" (AssignBlock "round3 = int(raw_input(Enter score for round 3: ))" (LoopBlock "A While n != guess" (Decision "guess < n" (IOBlock "print(guess)" (AssignBlock "guess = int(raw_input())" Next)) (Just (Decision "guess > n" (IOBlock "print(high)" (AssignBlock "guess = int(raw_input())" Next)) (Just (IOBlock "print(you)" Next)) Next)) (LoopEnd "A" (LoopBlock "A For x in fruits" (IOBlock "print(x)" (LoopEnd "A" (AssignBlock "average = (round1 + round2 + round3) / 3" (IOBlock "print(her, average)" (AssignBlock "a1 = e1" (AssignBlock "a2 = e2" (AssignBlock "a5 = e5" (End Nothing)))))))))))))),Start Nothing (AssignBlock "define function raw_input" (End Nothing)),Start Nothing (AssignBlock "define function a3" (End Nothing)),Start Nothing (AssignBlock "define function ia2" (End Nothing)),Start Nothing (AssignBlock "define function ia1" (End Nothing)),Start Nothing (AssignBlock "define function iia2" (End Nothing))]
