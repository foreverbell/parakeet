{-# LANGUAGE OverloadedStrings #-}

module Translator.Tex (
  tex
, texWrapped
) where

import           Control.Monad.Reader (asks)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Text.Printf (printf)
import           Prelude hiding (print)

import           Parser.Token (Token(..))
import           Monad.Parakeet (Parakeet)
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

texify :: [Token] -> Parakeet Text
texify ds = T.concat <$> mapM singleTexify ds
  where
    mainFont = 4
    rubyFont = 6
    romajiFont = 5
    singleTexify :: Token -> Parakeet Text
    singleTexify d = case d of
      Line         -> return " \\\\ \n"
      Break        -> do
        showBreak <- asks optShowBreak
        return $ if showBreak
          then "\\, "
          else T.empty
      Lit s        -> return $ T.pack $ build' True mainFont s ++ " "
      Kanji k h r  -> return $ T.pack $ printf "\\ruby{%s%s}{%s} " (build mainFont k) (build rubyFont ("(" ++ concat h ++ ")")) (build romajiFont (unwords r))
      Hiragana h r -> return $ T.pack $ printf "\\ruby{%s}{%s} " (build mainFont h) (build romajiFont (unwords r))
      Katakana k r -> return $ T.pack $ printf "\\ruby{%s}{%s} " (build mainFont k) (build romajiFont (unwords r))

tex :: [Token] -> Parakeet Text
tex cs = do
  body <- texify cs
  return $ T.concat [header, "\n\n", body]

texWrapped :: [Token] -> Parakeet Text
texWrapped cs = do
  body <- texify cs
  return $ T.unlines $ flip fmap tmpl $ \t -> 
    case t of
      "$body$" -> body
      _        -> t 
  where tmpl = header : map (T.filter (/= '\r')) (T.lines template)

