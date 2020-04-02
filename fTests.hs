import           Control.Applicative          ((<|>))
import           Data.Char
import           Text.ParserCombinators.ReadP

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
    ((char '\n' ) >> return ()) <|> eof
    return commentContent

spacesParser :: ReadP String
spacesParser = munch1 isSpace

skipCommentsAndSpaces :: ReadP ()
skipCommentsAndSpaces = skipMany (spacesParser <|> commentParser)
    s <- look
    skip s
  where
    skip (c:s) 
        | isSpace c = do _ <- get; skip s
        | c == '#'  = do 
            optional stringParser
            _ <- char '\n'
            skip s
    skip _                 = do return ()
