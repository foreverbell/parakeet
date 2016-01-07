{-# LANGUAGE OverloadedStrings #-}

module Parakeet.Translator.Intermediate (
  intermediate
) where

import           Control.Monad.Parakeet (Parakeet)
import           Control.Monad (forM)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Text.Printf (printf)
import           Prelude hiding (print)

import           Parakeet.Types.FToken 
import           Parakeet.Types.Meta 

escape :: String -> String
escape [] = []
escape ('{' : xs) = "{{" ++ escape xs
escape ('}' : xs) = "}}" ++ escape xs
escape (x : xs) = x : escape xs

tokenToText :: FToken -> Parakeet Text
tokenToText Line = return "\n"
tokenToText (Lit s) = return $ T.pack $ printf "\\lit{%s}" (escape s)
tokenToText (Kanji k h r) = return $ T.pack $ printf "\\kanji{%s}{%s}{%s}" k (concat h) (unwords r)
tokenToText (Hiragana h r) = return $ T.pack $ printf "\\hiragana{%s}{%s}" h (unwords r)
tokenToText (Katakana k r) = return $ T.pack $ printf "\\katakana{%s}{%s}" k (unwords r)

intermediate :: (Maybe Meta, [FToken]) -> Parakeet Text
intermediate (meta, tokens) = do
  title <- maybe (return T.empty) (\meta -> wrap "title" . tconcat <$> mapM tokenToText (getTitle meta)) meta
  author <- maybe (return T.empty) (\meta -> wrap "author" . tconcat <$> mapM tokenToText (getAuthor meta)) meta
  body <- tconcat <$> forM tokens tokenToText
  return $ title `lnappend` author `lnappend` body
  where 
    wrap tag text = T.concat ["\\", tag, "{", text, "}"]
    lnappend a b = if T.null a
      then b
      else T.concat [a, "\n", b]
    tconcat [] = T.empty
    tconcat ("\n":ts) = "\n" `T.append` tconcat ts
    tconcat (t:ts) = if T.null t then tconcat ts else T.concat [t, " ", tconcat ts]
