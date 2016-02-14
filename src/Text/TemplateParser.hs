module Text.TemplateParser (
  Chunk (..)
, templateParser
) where

import Text.Parsec

type Parser a = Parsec String () a

data Chunk 
  = Lit String
  | Value String
  deriving (Show, Eq)

templateParser :: Parser [Chunk]
templateParser = mergeLit <$> manyTill (lit <|> dollar <|> value) eof 
  where
    lit = Lit <$> many1 (satisfy (/= '$'))
    dollar = Lit <$> (try (string "$$") *> pure "$")
    value = Value <$> between (char '$') (char '$') var
      where var = many1 $ satisfy (/= '$')
    mergeLit = foldr f []
      where 
        f (Lit l) (Lit xl:xs) = Lit (l++xl) : xs 
        f x xs = x : xs
