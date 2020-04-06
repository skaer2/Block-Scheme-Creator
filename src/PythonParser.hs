module PythonParser where

import           Control.Applicative          ((<|>))
import           Data.Char
import           Text.ParserCombinators.ReadP

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
data Code =
    Code [Action]
    deriving (Show, Eq)

data Action
    = Assign Assignment
    | Call CallF
    | Definition Function
    deriving (Show, Eq)

--  | Use Keyword
data Assignment =
    Assignment Name Expr
    deriving (Show, Eq)

type Name = String

type Expr = String

data IndentType
    = Spaces
    | Tabs
    deriving (Show, Eq)

data CallF =
    CallF (Maybe SourceName) FunctionName [Argument]
    deriving (Show, Eq)

type SourceName = String

type FunctionName = String

type Argument = String

data Function =
    Function FunctionName [Argument] Code
    deriving (Show, Eq)

void a = a >> return ()

showResult :: [(a, String)] -> Maybe a
showResult r =
    case showResults r of
        []    -> Nothing
        (x:_) -> Just $ fst x

showResults :: [(a, String)] -> [(a, String)]
showResults =
    filter
        (\(x, leftovers) ->
             if leftovers == ""
                 then True
                 else False)

showShortOutput :: [(a, String)] -> [(a, String)]
showShortOutput = map (\(x, leftovers) -> (x, take 10 leftovers))

letter :: ReadP Char
letter = satisfy isAlpha

digit :: ReadP Char
digit = satisfy isDigit

number :: ReadP Int
number = fmap (read) $ many1 digit

isEndOfLine :: Char -> Bool
isEndOfLine = (==) '\n'

stringParser :: ReadP String 
stringParser = munch1 $ not . isEndOfLine 

nameParser :: ReadP String
nameParser = do
    firstLetter <- letter
    everythingElse <- many $ satisfy isAlowed
    return $ firstLetter : everythingElse
  where
    isAlowed = (||) <$> isAlphaNum <*> isUnderscore
    isUnderscore = (==) '_'

skipComments :: ReadP ()
skipComments = skipMany commentParser

commentParser :: ReadP String
commentParser = do
    char '#'
    commentContent <- option [] stringParser
    endOfLine <|> eof
    return commentContent

spacesParser :: ReadP String
spacesParser = munch1 isSpace

skipCommentsAndSpaces :: ReadP ()
skipCommentsAndSpaces = skipMany1 (endOfLine <|> skipComments)

endOfLine :: ReadP () -- TODO \ \n
endOfLine = void $ satisfy isEndOfLine

indentN :: Int -> IndentType -> ReadP [Char]
indentN n = count n . char . indentChar

indentChar Spaces = ' '
indentChar Tabs   = '\t'

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

codeEnd :: ReadP ()
codeEnd = (endOfLine <|> (void commentParser)) >> skipLines
  where
    skipLines = skipMany emptyLine
    emptyLine = do
        munch (\x -> elem x " \t")
        endOfLine <|> (void commentParser)

codeBlock oldN = do
    (n, t) <- begin oldN
    actions <- sepBy1 (indentN n t >> actionParser n) (codeEnd)
    optional codeEnd
    return $ Code actions
  where
    begin oldN = do
        ind@(newN, _) <- lookIndent
        if newN <= oldN
            then pfail
            else return ind

actionParser :: Int -> ReadP Action
actionParser n = simpleActParser <|> (compoundActParser n)

simpleActParser :: ReadP Action
simpleActParser = (fmap Assign assigmentParser) <|> (fmap Call callParser)

compoundActParser :: Int -> ReadP Action
compoundActParser n = (fmap Definition $ functionParser n)

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
