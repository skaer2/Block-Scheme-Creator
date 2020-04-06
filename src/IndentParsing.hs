module IndentParsing where

import           Text.ParserCombinators.ReadP

data IndentType
    = Spaces
    | Tabs
    deriving (Show, Eq)

indentChar :: IndentType -> Char
indentChar Spaces = ' '
indentChar Tabs   = '\t'

indentN :: Int -> IndentType -> ReadP [Char]
indentN n = count n . char . indentChar

lookTypeIndent :: IndentType -> ReadP (Int, IndentType)
lookTypeIndent t = do
    s <- look
    return (indent s, t)
  where
    indent (c:s)
        | (c == indentChar t) = 1 + indent s
    indent _ = 0

lookSpaceIndent :: ReadP (Int, IndentType)
lookSpaceIndent = lookTypeIndent Spaces

lookTabIndent :: ReadP (Int, IndentType)
lookTabIndent = lookTypeIndent Tabs

lookIndent :: ReadP (Int, IndentType)
lookIndent = do
    spaceInd@(nSpaces, _) <- lookSpaceIndent
    if nSpaces == 0
        then do
            tabInd <- lookTabIndent
            return tabInd
        else return spaceInd

