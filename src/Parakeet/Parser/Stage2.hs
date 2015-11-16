module Parakeet.Parser.Stage2 (
  stage2
) where

import           Control.Monad (when)
import           Control.Monad.Except (throwError)
import           Control.Monad.Parakeet

import           Parakeet.Parser.Token
import qualified Parakeet.Linguistics.Lexeme as L
import           Parakeet.Linguistics.Romaji (longVowelize)

splitToken :: Token -> ([L.Romaji] -> Token, [L.Romaji])
splitToken token = case token of
  Line -> (const Line, [])
  Break -> (const Break, [])
  Lit s -> (const (Lit s), [])
  Kanji k hs ks rs -> (Kanji k hs ks, rs)
  Hiragana h rs -> (Hiragana h, rs)
  Katakana k rs -> (Katakana k, rs)

-- parameter description:
-- if the next romaji is the part of a long vowel;
-- token builder;
-- the romajis to be processed of the current token;
-- the processed romajis of the current token;
-- the remaining tokens to be processed.
substitute :: Bool -> ([L.Romaji] -> Token) -> [L.Romaji] -> [L.Romaji] -> [Token] -> Parakeet [Token]
substitute False builder [] done [] = return [builder (reverse done)] 

substitute False builder (cur:curRest) done rest = if L.isRLV cur
  then substitute True builder curRest (mconcat (longVowelize True [cur]):done) rest
  else substitute False builder curRest (cur:done) rest

substitute False builder [] done (first:rest) = do
  let (builder', rs) = splitToken first
  next <- substitute False builder' rs [] rest
  return $ builder (reverse done) : next

substitute True _ [] _ [] = throwError internalError

substitute True builder (_:curRest) done rest = substitute False builder curRest done rest

substitute True builder [] done (first:rest) = do
  let (builder', rs) = splitToken first
  when (null rs) $ throwError internalError
  next <- substitute False builder' (tail rs) [] rest
  return $ builder (reverse done) : next

stage2 :: [Token] -> Parakeet [Token]
stage2 [] = return []
stage2 tokens = do
  let (builder, rs) = splitToken (head tokens)
  substitute False builder rs [] (tail tokens)

internalError :: String
internalError = "[internal error] except an vowel romaji but not found"
