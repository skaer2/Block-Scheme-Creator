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

letter :: ReadP Char
letter = satisfy isAlpha

digit :: ReadP Char
digit = satisfy isDigit

number :: ReadP Int
number = fmap (read) $ many1 digit

isEndOfLine :: Char -> Bool
isEndOfLine = (==) '\n'

stringParser :: ReadP String
stringParser = many1 $ satisfy $ not . isEndOfLine

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
    ((char '\n' ) >> return ()) <|> eof
    return commentContent

spacesParser :: ReadP String
spacesParser = munch1 isSpace

skipCommentsAndSpaces :: ReadP ()
skipCommentsAndSpaces = skipMany (spacesParser <|> commentParser)

endOfLine :: ReadP () -- TODO \ \n
endOfLine = (satisfy isEndOfLine >> return ()) <|> eof

indentTabs :: ReadP ([Char], IndentType)
indentTabs = do
    chars <- many1 (char '\t') 
    return (chars, Tabs)

indentSpaces :: ReadP ([Char], IndentType)
indentSpaces = do
    chars <- many1 (char ' ') 
    return (chars, Spaces)

getIndentSizeAndType :: ReadP (Int, IndentType)
getIndentSizeAndType = do
    (chars, indType) <- (indentTabs <|> indentSpaces)
    return (length chars, indType)

indentN :: Int -> IndentType -> ReadP [Char] 
indentN n = count n . char . indentChar
    where
        indentChar Spaces = ' '
        indentChar Tabs = '\t'

codeEnd :: ReadP ()
codeEnd = skipCommentsAndSpaces <|> eof 

codeParser :: Maybe Int -> ReadP Code
codeParser n = do
    (n', indType) <- option (0, Spaces) getIndentSizeAndType
    action <- actionParser 
    codeEnd
    otherActions <- sepBy (indentN n' indType >> actionParser) (codeEnd)
    return $ Code (action:otherActions)

--codeParser :: Int -> ReadP Code
--codeParser n = fmap Code $ sepBy1 (actionParser n) (endOfLine n >> skipSpaces)

actionParser :: ReadP Action
actionParser =
    (fmap Assign assigmentParser) <|> (fmap Call callParser) <|> (fmap Definition $ functionParser)

assigmentParser :: ReadP Assignment
assigmentParser = do
    name <- nameParser
    skipSpaces
    char '='
    skipSpaces
    expr <- stringParser
    return $ Assignment name expr

callParser :: ReadP CallF
callParser = do
    sourceName <- option Nothing source
    functionName <- nameParser
    char '('
    arguments <- sepBy stringParser (skipSpaces >> char ',' >> skipSpaces)
    char ')'
    return $ CallF sourceName functionName arguments
  where
    source = do
        name <- nameParser
        char '.'
        return $ Just name

functionParser :: ReadP Function
functionParser = do
    string "def"
    skipSpaces
    functionName <- nameParser
    skipSpaces
    char '('
    arguments <- sepBy stringParser (skipSpaces >> char ',' >> skipSpaces)
    char ')'
    char ':'
    codeEnd
    code <- codeParser Nothing
    return $ Function functionName arguments code
