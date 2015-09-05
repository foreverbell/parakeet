{-# LANGUAGE OverloadedStrings #-}

module Print (
  prettyPrint
) where

import           Control.Monad.Reader (asks)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Data.List (intercalate)
import           Text.Printf (printf)
import           Prelude hiding (print)

import           Parser.Parser (parse)
import           Token.Compound (Compound(..))
import           Monad.Parakeet (Parakeet, runParakeet)
import           Options (Options(..))
import           Template (template, header)

build' :: Bool -> Int -> String -> String
build' verb f
  | verb      = printf "\\%s{\\verb|%s|}" (fonts !! f)
  | otherwise = printf "\\%s{%s}" (fonts !! f)
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

build = build' False

texify :: [Compound] -> Parakeet Text
texify ds = T.concat <$> mapM singleTexify ds
  where
    mainFont = 4
    rubyFont = 6
    romajiFont = 5
    singleTexify :: Compound -> Parakeet Text
    singleTexify d = case d of
      Line         -> return $ T.pack $ " \\\\ \n"
      Break        -> do
        showBreak <- asks optShowBreak
        return $ if showBreak
          then T.pack $ "\\, "
          else T.empty
      Lit s        -> return $ T.pack $ build' True mainFont s ++ " "
      Kanji k h r  -> return $ T.pack $ printf "\\ruby{%s%s}{%s} " (build mainFont k) (build rubyFont ("(" ++ concat h ++ ")")) (build romajiFont (intercalate " " r))
      Hiragana h r -> return $ T.pack $ printf "\\ruby{%s}{%s} " (build mainFont h) (build romajiFont (intercalate " " r))
      Katakana k r -> return $ T.pack $ printf "\\ruby{%s}{%s} " (build mainFont k) (build romajiFont (intercalate " " r))

body :: Parakeet Text
body = texify =<< parse
 
wrap :: Text -> Text -> Parakeet Text
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

print :: Parakeet Text
print = wrap header =<< body

prettyPrint :: Options -> Either String String
prettyPrint opts = T.unpack <$> runParakeet opts print
