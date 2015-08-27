{-# LANGUAGE OverloadedStrings #-}

module Print (
  prettyPrint
) where

import           Control.Applicative ((<$>))
import           Control.Monad.Reader (asks)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Data.List (intercalate)
import           Text.Printf (printf)
import           Prelude hiding (print)

import           Parser.Parser (parse)
import           Token.Compound (Compound(..))
import           Eval (Eval, runEval)
import           Options (Options(..))
import           Template (template, header)

build :: Int -> String -> String
build f = printf "\\%s{%s}" (fonts !! f)
  where fonts = [ "Huge"
                , "huge"
                , "LARGE"
                , "Large"
                , "large"
                , "normalsize"
                , "small"
                , "footnotesize"
                , "scriptsize"
                , "tiny" 
                ] :: [String]

texify :: [Compound] -> Eval Text
texify ds = T.concat <$> mapM singleTexify ds
  where
    mainFont = 4
    rubyFont = 6
    romajiFont = 5
    singleTexify d = case d of
      Line         -> return $ T.pack $ " \\\\ \n"
      Break        -> do
        showBreak <- asks optShowBreak
        return $ if showBreak
          then T.pack $ "\\, "
          else T.empty
      Lit s        -> return $ T.pack $ build mainFont s ++ " "
      Kanji k h r  -> return $ T.pack $ printf "\\ruby{%s%s}{%s} " (build mainFont k) (build rubyFont ("(" ++ concat h ++ ")")) (build romajiFont (intercalate " " r))
      Hiragana h r -> return $ T.pack $ printf "\\ruby{%s}{%s} " (build mainFont h) (build romajiFont (intercalate " " r))
      Katakana k r -> return $ T.pack $ printf "\\ruby{%s}{%s} " (build mainFont k) (build romajiFont (intercalate " " r))

body :: Eval Text
body = texify =<< parse
 
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
