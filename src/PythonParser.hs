module PythonParser where

import           Control.Applicative          ((<|>))
import           Data.Char
import           Text.ParserCombinators.ReadP

import IndentParsing 
import ASTTypes
import HelperFunctions

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
-- Expr = String
--
-- Function = Def Name '(' Arguments ')' ':'
--            BlockBegin Code BlockEnd
-- BlockBegin = IndentIn
-- BlockEnd   = IndentOut
--

importParser :: ReadP ()
importParser = do
    string "import"
    void stringParser

nameParser :: ReadP String
nameParser = do
    firstLetter <- letter
    everythingElse <- many $ satisfy isAlowed
    return $ firstLetter : everythingElse
  where
    isAlowed = (||) <$> isAlphaNum <*> isUnderscore
    isUnderscore = (==) '_'

commentParser :: ReadP String
commentParser = do
    char '#'
    commentContent <- option [] stringParser
    endOfLine <|> eof
    return commentContent

codeEnd :: ReadP ()
codeEnd = (endOfLine <|> (void commentParser)) >> skipLines
  where
    skipLines = skipMany emptyLine
    emptyLine = do
        munch (\x -> elem x " \t")
        endOfLine <|> (void commentParser)

codeBlock :: Int -> ReadP Code
codeBlock oldN = do
    ind <- begin oldN
    actions <- sepBy1 (indentN ind >> actionParser ind) (codeEnd)
    optional codeEnd
    return $ Code actions
  where
    begin oldN = do
        ind@(newN, _) <- lookIndent
        if newN <= oldN
            then pfail
            else return ind

actionParser :: Indent -> ReadP Action
actionParser ind = simpleActParser <|> (compoundActParser ind)

simpleActParser :: ReadP Action
simpleActParser = (fmap Assign assigmentParser) <|> (fmap Call callParser)

compoundActParser :: Indent -> ReadP Action
compoundActParser ind@(n, _) = (fmap Def $ functionParser n) <|> (fmap IfBlock $ ifParser ind) 

assigmentParser :: ReadP Assignment
assigmentParser = do
    name <- nameParser
    skipSpaces
    char '='
    skipSpaces
    expr <- stringParser
    return $ Assignment name expr

argsParser :: ReadP [Argument]
argsParser = sepBy nameParser argsSeparator

argsSeparator :: ReadP ()
argsSeparator = skipSpaces >> char ',' >> skipSpaces

callParser :: ReadP CallF
callParser = do
    sourceName <- option Nothing source
    functionName <- nameParser
    char '('
    arguments <- sepBy callArgP argsSeparator
    char ')'
    return $ CallF sourceName functionName arguments
  where
    callArgP = munch1 $ not . (flip elem) "\n(),"
    source = do
        name <- nameParser
        char '.'
        return $ Just name

functionParser :: Int -> ReadP Function
functionParser n = do
    string "def"
    skipSpaces
    functionName <- nameParser
    skipSpaces
    char '('
    arguments <- argsParser
    char ')'
    char ':'
    codeEnd
    code <- codeBlock n
    codeEnd
    return $ Function functionName arguments code

conditionParser :: ReadP Condition 
conditionParser = munch1 $ not . (flip elem) ":"

elseParser :: Int -> ReadP (Maybe Else)
elseParser n = do
    string "else:"
    codeEnd
    code <- codeBlock n 
    codeEnd
    return $ Just $ Else code

elifParser :: Indent -> ReadP (Maybe Else)
elifParser ind = do
    string "el"
    ifCode <- ifParser ind
    return $ Just $ Else $ Code [IfBlock ifCode]

ifParser :: Indent -> ReadP If
ifParser ind@(n, _) = do
    string "if"
    skipSpaces
    condition <- conditionParser
    char ':'
    codeEnd
    code <- codeBlock n 
    codeEnd
    indentN ind
    elseCode <- option Nothing (elseParser n <|> elifParser ind)
    return $ If condition code elseCode

