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

import           Parser.Token (Token(..))
import           Monad.Parakeet (Parakeet)
import           Options (Options(..))

escape :: String -> String
escape [] = []
escape ('{' : xs) = "{{" ++ escape xs
escape ('}' : xs) = "}}" ++ escape xs
escape (x : xs) = x : escape xs

intermediate :: [Token] -> Parakeet Text
intermediate ds = flatten <$> ts
  where 
    ts = forM ds $ \d -> 
      case d of 
        Line -> return "\n"
        Break -> do
          showBreak <- asks optShowBreak
          return $ if showBreak 
            then "\\break"
            else T.empty
        Lit s -> return $ T.pack $ printf "\\lit{%s}" (escape s)
        Kanji k h r -> return $ T.pack $ printf "\\kanji{%s}{%s}{%s}" k (concat h) (unwords r)
        Hiragana h r -> return $ T.pack $ printf "\\hiragana{%s}{%s}" h (unwords r)
        Katakana k r -> return $ T.pack $ printf "\\katakana{%s}{%s}" k (unwords r)
    flatten [] = T.empty
    flatten ("\n" : ts) = "\n" `T.append` flatten ts
    flatten (t : ts) = if T.null t
      then flatten ts
      else t `T.append` " " `T.append` flatten ts
