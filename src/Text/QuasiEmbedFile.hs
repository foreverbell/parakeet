{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Text.QuasiEmbedFile (
  efile
, rfile
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec (runParser)
import Text.TemplateParser

parseQuoteExp :: FilePath -> Q Exp
parseQuoteExp f = do
  c <- runIO $ readFile f
  case runParser templateParser () f c of
       Right chunks -> listE $
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


rfile :: QuasiQuoter
rfile = QuasiQuoter { quoteExp  = rawFileExp
                    , quotePat  = undefined
                    , quoteType = undefined
                    , quoteDec  = undefined
                    }
  where
    rawFileExp :: FilePath -> Q Exp
    rawFileExp f = do
      c <- runIO $ readFile f
      [| c |]
