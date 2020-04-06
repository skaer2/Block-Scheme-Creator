module HelperFunctions where 

import           Data.Char
import           Text.ParserCombinators.ReadP

void :: Monad m => m a -> m ()
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

endOfLine :: ReadP () -- TODO \ \n
endOfLine = void $ satisfy isEndOfLine
