import           Control.Applicative          ((<|>))
import           Data.Char
import           Text.ParserCombinators.ReadP

data IndentType 
    = Spaces
    | Tabs
    deriving (Show, Eq)

indentChar Spaces = ' '
indentChar Tabs = '\t'

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

isEndOfLine :: Char -> Bool
isEndOfLine = (==) '\n'

stringParser :: ReadP String
stringParser = many1 $ satisfy $ not . isEndOfLine

skipComments :: ReadP ()
skipComments = skipMany commentParser

commentParser :: ReadP String
commentParser = do
    char '#' 
    commentContent <- option [] stringParser 
    endOfLine
    return commentContent

spacesParser :: ReadP String
spacesParser = munch1 isSpace

--codeBlock

    --where
        --begin = do
            --newN <- lookSpaceIndent
            --if newN <= n then pfail
                --else return newN
                
void :: Monad m => m a -> m ()
void a = a >> return ()

endOfLine :: ReadP () -- TODO \ \n
endOfLine = void $ satisfy isEndOfLine

codeEnd :: ReadP ()
codeEnd = (endOfLine <|> (void commentParser)) >> skipLines
  where
    skipLines = skipMany emptyLine
    emptyLine = do 
        munch (\x -> elem x " \t")
        endOfLine <|> (void commentParser)

lookTypeIndent :: IndentType -> ReadP (Int, IndentType)
lookTypeIndent t = do
    s <- look
    return (indent s, t)
  where
      indent (c:s) | (c == indentChar t) = 1 + indent s 
      indent _ = 0

lookSpaceIndent :: ReadP (Int, IndentType)
lookSpaceIndent = lookTypeIndent Spaces

lookTabIndent :: ReadP (Int, IndentType)
lookTabIndent = lookTypeIndent Tabs

lookIndent :: ReadP (Int, IndentType)
lookIndent = do
    spaceInd@(nSpaces, _) <- lookSpaceIndent 
    if nSpaces == 0 then do 
        tabInd <- lookTabIndent
        return tabInd
    else return spaceInd

--skipCommentsAndSpaces :: ReadP ()
--skipCommentsAndSpaces = skipMany (spacesParser <|> commentParser)
    --s <- look
    --skip s
  --where
    --skip (c:s) 
        -- | isSpace c = do _ <- get; skip s
        -- | c == '#'  = do 
            --optional stringParser
            --_ <- char '\n'
            --skip s
    --skip _                 = do return ()
