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
    chars <- many (char '\t') 
    return (chars, Tabs)

indentSpaces :: ReadP ([Char], IndentType)
indentSpaces = do
    chars <- many (char ' ') 
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

altCodeParser :: Maybe Int -> ReadP Code
altCodeParser n = do
    (n', indType) <- codeStart n Spaces
    action <- actionParser 
    codeEnd
    otherActions <- sepBy (codeStart n' indType >> actionParser) (codeEnd)
    return $ action:otherActions
    where
        codeStart :: Maybe Int -> IndentType -> ReadP (Int, IndentType)
        codeStart (Just n') indType = indentN n' indType >> return ()
        codeStart (Nothing) _ = getIndentSizeAndType
        codeEnd = (skipCommentsAndSpaces) <++ (endOfLine >> skipCommentsAndSpaces) 

--codeParser :: Int -> ReadP Code
--codeParser n = fmap Code $ sepBy1 (actionParser n) (endOfLine n >> skipSpaces)

actionParser :: Int -> ReadP Action
actionParser n =
    (fmap Assign assigmentParser) <|> (fmap Call callParser) <|> (fmap Definition $ functionParser n)

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

functionParser :: Int -> ReadP Function
functionParser n = do
    string "def"
    skipSpaces
    functionName <- nameParser
    skipSpaces
    char '('
    arguments <- sepBy stringParser (skipSpaces >> char ',' >> skipSpaces)
    char ')'
    char ':'
    endOfLine $ n + 1
    code <- codeParser $ n + 1
    return $ Function functionName arguments code
