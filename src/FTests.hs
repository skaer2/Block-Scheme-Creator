module FTests where

import           ASTTypes
import           Control.Applicative          ((<|>))
import           Data.Char
import           Data.List
import           HelperFunctions
import           Text.ParserCombinators.ReadP

getInnerActions :: Action -> (Maybe [Action], Maybe [Action])
getInnerActions (Def (Function _ _ (Code ias))) = (Just ias, Nothing)
getInnerActions (LoopW (While cond (Code ias))) = (Just ias, Nothing)
getInnerActions (LoopF (For var cond (Code ias))) = (Just ias, Nothing)
getInnerActions (IfBlock (If cond (Code ias) e)) =
    case e of
        Nothing                -> (Just ias, Nothing)
        Just (Else (Code eas)) -> (Just ias, Just eas)
getInnerActions _ = (Nothing, Nothing)

mapInnerActions :: ([Action] -> [Action]) -> Action -> Action
mapInnerActions f (Def (Function fn args (Code ias))) = Def (Function fn args (Code $ f ias))
mapInnerActions f (LoopW (While cond (Code ias))) = LoopW (While cond (Code $ f ias))
mapInnerActions f (LoopF (For var cond (Code ias))) = LoopF (For var cond (Code $ f ias))
mapInnerActions f (IfBlock (If cond (Code ias) e)) =
    case e of
        Nothing                -> IfBlock (If cond (Code $ f ias) Nothing)
        Just (Else (Code eas)) -> IfBlock (If cond (Code $ f ias) (Just (Else (Code $ f eas))))
mapInnerActions _ a = a

removeEmptyCodes :: [Action] -> [Action]
removeEmptyCodes [] = []
removeEmptyCodes (a:as) =
    case (getInnerActions a) of
        (Just [], _)   -> removeEmptyCodes as
        (Nothing, _)   -> a : removeEmptyCodes as
        (Just (xs), _) -> (mapInnerActions removeEmptyCodes a) : removeEmptyCodes as

removeDefs :: [Action] -> ([Function], [Action])
removeDefs [] = ([], [])
removeDefs ((Def (Function fn args (Code ias))):as) =
    ( (Function fn args (Code $ snd newIas)) : (fst $ newIas) ++ (fst $ removeDefs as)
    , snd $ removeDefs as)
  where
    newIas = removeDefs ias
removeDefs ((LoopW (While cond (Code ias))):as) =
    ( (fst $ removeDefs as) ++ (fst newIas)
    , (LoopW (While cond (Code (snd newIas)))) : (snd $ removeDefs as))
  where
    newIas = removeDefs ias
removeDefs ((LoopF (For var cond (Code ias))):as) =
    ( (fst $ removeDefs as) ++ (fst newIas)
    , (LoopF (For var cond (Code (snd newIas)))) : (snd $ removeDefs as))
  where
    newIas = removeDefs ias
removeDefs ((IfBlock (If cond (Code ias) e)):as) =
    ( (fst $ removeDefs as) ++ (fst newIas) ++ (fst newE)
    , (IfBlock (If cond (Code (snd newIas)) wrappedNewE)) : (snd $ removeDefs as))
  where
    newE =
        case e of
            Nothing                -> ([], [])
            Just (Else (Code eas)) -> removeDefs eas
    wrappedNewE =
        case e of
            Nothing                -> Nothing
            Just (Else (Code eas)) -> Just (Else $ Code (snd $ removeDefs eas))
    newIas = removeDefs ias
removeDefs (a:as) = (fst $ removeDefs as, a : (snd $ removeDefs as))

readyProgramm :: Programm -> (Programm, [Action])
readyProgramm (Programm (Code acts)) = (wrappedLeft, wrappedRight)
  where
    wrappedLeft = Programm (Code (fst splitClean))
    wrappedRight = snd splitClean
    splitClean :: ([Action], [Action])
    splitClean = (removeEmptyCodes left, removeEmptyCodes right)
    right = map (Def) $ fst $ removeDefs acts
    left = snd $ removeDefs acts
