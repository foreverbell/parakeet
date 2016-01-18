{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Text.QuasiEmbedFile (
  efile
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Parsec

type Parser a = Parsec String () a

data Chunk 
  = Lit String
  | Value String
  deriving (Show, Eq)

parser :: Parser [Chunk]
parser = manyTill (lit <|> value) eof 
  where
    lit = Lit <$> (many1 $ satisfy (/= '$'))
    value = Value <$> (between (char '$') (char '$') var)
      where
        var = many1 $ satisfy (/= '$')
    
parseQuoteExp :: FilePath -> Q Exp
parseQuoteExp f = do
  c <- runIO $ readFile f
  case runParser parser () f c of
       Right chunks -> listE $ do
         flip map chunks $ \elem ->
           case elem of
                Lit lit -> [| lit |]
                Value v -> varE (mkName v)
       Left err -> fail ("invalid template file\n" ++ show err)
  
efile :: QuasiQuoter
efile = QuasiQuoter { quoteExp  = parseQuoteExp 
                    , quotePat  = undefined
                    , quoteType = undefined
                    , quoteDec  = undefined
                    }
