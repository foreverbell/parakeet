{-# LANGUAGE OverloadedStrings #-}

module Parakeet.Translator.Intermediate (
  intermediate
) where

import           Control.Monad.Parakeet (Parakeet)
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

tokenToText :: FToken -> Text
tokenToText Line = "\n"
tokenToText (Lit s) = T.pack $ printf "\\lit{%s}" (escape s)
tokenToText (Kanji k h r) = T.pack $ printf "\\kanji{%s}{%s}{%s}" k (concat h) (unwords r)
tokenToText (Hiragana h r) = T.pack $ printf "\\hiragana{%s}{%s}" h (unwords r)
tokenToText (Katakana k r) = T.pack $ printf "\\katakana{%s}{%s}" k (unwords r)

intermediate :: (Maybe Meta, [FToken]) -> Parakeet Text
intermediate (meta, tokens) = do
  let title = maybe T.empty (\meta -> wrap "title" $ tconcat $ map tokenToText (getTitle meta)) meta
  let author = maybe T.empty (\meta -> wrap "author" $ tconcat $ map tokenToText (getAuthor meta)) meta
  let body = tconcat $ map tokenToText tokens
  return $ title `lnappend` author `lnappend` body
  where 
    wrap tag text = T.concat ["\\", tag, "{", text, "}"]
    lnappend a b | T.null a = b
                 | otherwise = T.concat [a, "\n", b]
    tconcat [] = T.empty
    tconcat ("\n":ts) = "\n" `T.append` tconcat ts
    tconcat (t:ts) = if T.null t then tconcat ts else T.concat [t, " ", tconcat ts]
