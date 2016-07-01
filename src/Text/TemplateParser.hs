module Text.TemplateParser (
  Chunk (..)
, parser
, substitute
) where

import           Control.Monad.Parakeet (Parakeet, TemplateError (..), toException, throw)

import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Text.Parsec
import           Text.Printf (printf)
type Parser a = Parsec String () a

data Chunk 
  = Lit String
  | Value String
  deriving (Show, Eq)

parser :: Parser [Chunk]
parser = mergeLit <$> manyTill (lit <|> dollar <|> value) eof 
  where
    lit = Lit <$> many1 (satisfy (/= '$'))
    dollar = Lit <$> (try (string "$$") *> pure "$")
    value = Value <$> between (char '$') (char '$') var
      where var = many1 $ satisfy (/= '$')
    mergeLit = foldr f []
      where 
        f (Lit l) (Lit xl:xs) = Lit (l++xl) : xs 
        f x xs = x : xs

substitute :: String -> Text -> Text -> Text -> Parakeet Text
substitute template body title author = do
  chunks <- case runParser parser () [] template of
              Right chunks -> return chunks
              Left err -> throw $ toException (TemplateError $ printf "invalid template: %s." (show err))
  T.concat <$> mapM go chunks
  where
    go (Lit l) = return $ T.pack l
    go (Value v) = case v of
                     "title" -> return title
                     "author" -> return author
                     "body" -> return body
                     _ -> throw $ toException (TemplateError $ printf "invalid placeholder $%s$." v)
