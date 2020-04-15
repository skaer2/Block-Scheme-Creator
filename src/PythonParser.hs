module PythonParser where

--TODO add assignment with modifiers: 
--          data Modifier = ...
--
--          data Assignment = ... Modifier ...
--
--          ... modifierParser ...
--          mod <- modifierParser
--          return Assignment ... mod ... 

import           Control.Applicative          ((<|>))
import           Data.Char
import           Text.ParserCombinators.ReadP

import           ASTTypes
import           HelperFunctions
import           IndentParsing

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
parsePython :: String -> Either String Programm
parsePython s =
    case (result) of
        Nothing -> Left "unexpected error"
        Just x ->
            case x of
                (_, []) -> Right $ fst x
                _       -> Left $ snd x
  where
    result = lastMaybe $ readP_to_S pythonParser s

pythonParser :: ReadP Programm
pythonParser = fmap Programm $ (skipMany importParser >> codeBlock (-1))

flowActParser :: ReadP FlowAct
flowActParser = breakParser <|> continueParser <|> returnParser <|> yieldParser 
  where
    breakParser = string "break" >> return Break
    continueParser = string "continue" >> return Continue
    returnParser = xParser "return" (Return)
    yieldParser = xParser "yield" (Yield)
    xParser x f = string x >> skipSpaces >> do 
        s <- stringParser 
        return $ f s
    

importParser :: ReadP ()
importParser = do
    string "import"
    void stringParser
    codeEnd

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
    endOfLine 
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
actionParser ind = (fmap Flow flowActParser) <|> simpleActParser <|> (compoundActParser ind)

simpleActParser :: ReadP Action
simpleActParser = (fmap Assign assigmentParser) <|> (fmap Call callParser)

compoundActParser :: Indent -> ReadP Action
compoundActParser ind@(n, _) =
    (fmap Def $ functionParser n) <|> (fmap IfBlock $ ifParser ind) <|> (fmap LoopW $ whileParser n) <|>
    (fmap LoopF $ forParser n)

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
    return $ Function functionName arguments code

conditionParser :: ReadP Condition
conditionParser = munch1 $ not . (flip elem) ":"

elseParser :: Int -> ReadP (Maybe Else)
elseParser n = do
    string "else:"
    codeEnd
    code <- codeBlock n
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
    skipSpaces
    char ':'
    codeEnd
    code <- codeBlock n
    elseCode <- option Nothing (indentN ind >> (elseParser n <|> elifParser ind))
    return $ If condition code elseCode

whileParser :: Int -> ReadP While
whileParser n = do
    string "while"
    skipSpaces
    condition <- conditionParser
    skipSpaces
    char ':'
    codeEnd
    code <- codeBlock n
    return $ While condition code

forParser :: Int -> ReadP For
forParser n = do
    string "for"
    skipSpaces
    variable <- nameParser
    skipSpaces
    string "in"
    skipSpaces
    condition <- conditionParser
    skipSpaces
    char ':'
    codeEnd
    code <- codeBlock n
    return $ For variable condition code
