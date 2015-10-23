{-# LANGUAGE OverloadedStrings #-}

module Translator.Intermediate (
  intermediate
) where

import           Control.Monad.Reader (asks)
import           Control.Monad (forM)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Text.Printf (printf)
import           Prelude hiding (print)

import           Parser.FlatToken (FlatToken(..))
import           Parser.MetaInfo (MetaInfo(..), getTitle, getAuthor)
import           Monad.Parakeet (Parakeet)
import           Options (Options(..))

escape :: String -> String
escape [] = []
escape ('{' : xs) = "{{" ++ escape xs
escape ('}' : xs) = "}}" ++ escape xs
escape (x : xs) = x : escape xs

smartConcat :: [Text] -> Text
smartConcat [] = T.empty
smartConcat ("\n":ts) = "\n" `T.append` smartConcat ts
smartConcat (t:ts) = if T.null t then smartConcat ts else T.concat [t, " ", smartConcat ts]

tokenToText :: FlatToken -> Parakeet Text
tokenToText Line = return "\n"
tokenToText Break = do
  showBreak <- asks optShowBreak
  return $ if showBreak 
    then "\\break"
    else T.empty
tokenToText (Lit s) = return $ T.pack $ printf "\\lit{%s}" (escape s)
tokenToText (Kanji k h r) = return $ T.pack $ printf "\\kanji{%s}{%s}{%s}" k (concat h) (unwords r)
tokenToText (Hiragana h r) = return $ T.pack $ printf "\\hiragana{%s}{%s}" h (unwords r)
tokenToText (Katakana k r) = return $ T.pack $ printf "\\katakana{%s}{%s}" k (unwords r)

intermediate :: (Maybe MetaInfo, [FlatToken]) -> Parakeet Text
intermediate (meta, tokens) = do
  title <- maybe (return T.empty) (\meta -> wrap "title" . smartConcat <$> mapM tokenToText (getTitle meta)) meta
  author <- maybe (return T.empty) (\meta -> wrap "author" . smartConcat <$> mapM tokenToText (getAuthor meta)) meta
  body <- smartConcat <$> forM tokens tokenToText
  return $ title `lnappend` author `lnappend` body
  where 
    wrap tag text = T.concat ["\\", tag, "{", text, "}"]
    lnappend a b = if T.null a
      then b
      else T.concat [a, "\n", b]
