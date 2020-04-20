module FTests where

import           ASTTypes
import           Control.Applicative          ((<|>))
import           Data.Char
import           Data.List
import           HelperFunctions
import           Text.ParserCombinators.ReadP

removeDefs :: [Action] -> ([Function], [Action])
removeDefs [] = ([], [])
removeDefs ((Def (Function fn args (Code ias))):as) =
    ((Function fn args (Code $ snd newIas)) : (fst $ newIas) ++ (fst $ removeDefs as), snd $ removeDefs as)
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

f1 acts = putStrLn $ addLineBreaks $ show $ removeDefs acts
