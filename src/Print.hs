{-# LANGUAGE OverloadedStrings #-}

module Print (
  prettyPrint
) where

import           Control.Applicative ((<$>))
import           Control.Monad.Reader (asks)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Prelude hiding (print)

import           Eval (Eval, runEval)
import           Options (Options(..))
import           Template (template, header)
import           Parser.Parser (parse)
import qualified Element as E

body :: Eval Text
body = E.texify =<< parse
 
wrap :: Text -> Text -> Eval Text
wrap hder body = do
  noWrap <- asks optNoWrap
  return $ if (noWrap)
    then T.concat [hder, "\n\n", body]
    else T.unlines $ flip fmap tmpl $ \t -> 
           case t of
             "$body$" -> body
             _        -> t 
    where
      tmpl = hder : map (T.filter (/= '\r')) (T.lines template)

print :: Eval Text
print = wrap header =<< body

prettyPrint :: Options -> Either String String
prettyPrint opts = T.unpack <$> runEval opts print
