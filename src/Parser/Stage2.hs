module Parser.Stage2 (
  stage2
) where

import Control.Monad (when)

import           Parser.Token
import           Monad.Parakeet
import qualified Linguistics.Lexeme as L
import           Linguistics.Romaji (longVowelize)

splitToken :: Token -> ([L.Romaji] -> Token, [L.Romaji])
splitToken token = case token of
  Line -> (const Line, [])
  Break -> (const Break, [])
  Lit s -> (const (Lit s), [])
  Kanji k hs ks rs -> (Kanji k hs ks, rs)
  Hiragana h rs -> (Hiragana h, rs)
  Katakana k rs -> (Katakana k, rs)

-- TODO: Test this messy code, and fix all `undefined`
-- toEat -> builder -> cur -> curDone -> rest
go :: Bool -> ([L.Romaji] -> Token) -> [L.Romaji] -> [L.Romaji] -> [Token] -> Parakeet [Token]
go False builder [] done [] = return [builder (reverse done)] 
go False builder (cur:curRest) done rest = if L.isRLV cur
  then go True builder curRest (mconcat (longVowelize True [cur]):done) rest
  else go False builder curRest (cur:done) rest
go False builder [] done (first:rest) = do
  let (builder', rs) = splitToken first
  next <- go False builder' rs [] rest
  return $ builder (reverse done) : next
go True _ [] _ [] = undefined
go True builder (_:curRest) done rest = go False builder curRest done rest
go True builder [] done (first:rest) = do
  let (builder', rs) = splitToken first
  when (null rs) undefined
  next <- go False builder' (tail rs) [] rest
  return $ builder (reverse done) : next

stage2 :: [Token] -> Parakeet [Token]
stage2 [] = return []
stage2 tokens = do
  let (builder, rs) = splitToken (head tokens)
  go False builder rs [] (tail tokens)
