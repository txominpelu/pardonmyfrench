module PardonMyFrench.FrenchParser where

import Text.ParserCombinators.Parsec

-- data ErroredWord = Alternative String String deriving (Show)
data Word = Correct String | ErroredWord String String

instance Show Word where
    show (ErroredWord error correct)   = "[" ++ error ++ "|" ++ correct ++ "]"
    show (Correct word) = word

data Sentence = Sentence [Word] deriving (Show)

sentence :: Parser Sentence
sentence = 
    do result <- mword
       eof
       return (Sentence result)

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
mword :: Parser [Word]
mword = (sepBy ( choice [(try erroredWord), (fmap Correct (many1 frenchAlphaNum))] ) (many space))

frenchAlphaNum :: Parser Char
frenchAlphaNum = oneOf "'-?" <|> letter

erroredWord :: Parser Word
erroredWord = do prefix <- many frenchAlphaNum
                 char '['
                 w1 <- many1 frenchAlphaNum
                 char '|'
                 w2 <- many1 frenchAlphaNum
                 char ']'
                 postfix <- many frenchAlphaNum
                 return (ErroredWord (prefix ++ w1 ++ postfix) (prefix ++ w2 ++ postfix))

