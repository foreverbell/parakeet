module Parakeet.Parser.Stage2 (
  stage2
) where

import           Control.Monad (when)
import           Control.Monad.Parakeet (Parakeet, throw)

import           Parakeet.Types.Token
import qualified Parakeet.Types.Lexeme as L
import           Parakeet.Linguistics.Romaji (longVowelize1)

splitToken :: Token -> ([L.Romaji L.Single] -> Token, [L.Romaji L.Single])
splitToken token = case token of
  Line -> (const Line, [])
  Break -> (const Break, [])
  Lit s -> (const (Lit s), [])
  Kanji k hs ks rs -> (Kanji k hs ks, rs)
  Hiragana h rs -> (Hiragana h, rs)
  Katakana k rs -> (Katakana k, rs)

{-
 - if the next romaji is the part of a long vowel
 - token builder
 - romajis wait to be processed of the current token
 - processed romajis of the current token
 - remaining tokens to be processed
 -}
substitute :: Bool -> ([L.Romaji L.Single] -> Token) -> [L.Romaji L.Single] -> [L.Romaji L.Single] -> [Token] -> Parakeet [Token]
substitute False b [] d [] = return [b (reverse d)] 

substitute False b (c:r) d r0 = if L.isRLV c
  then substitute True b r (longVowelize1 c:d) r0
  else substitute False b r (c:d) r0

substitute False b [] d r0 = do
  let (b', rs) = splitToken (head r0)
  n <- substitute False b' rs [] (tail r0)
  return $ b (reverse d) : n

substitute True _ [] _ [] = throw internalError

substitute True b (_:r) d r0 = substitute False b r d r0

substitute True b [] d r0 = do
  let (b', rs) = splitToken (head r0)
  when (null rs) $ throw internalError
  n <- substitute False b' (tail rs) [] (tail r0)
  return $ b (reverse d) : n

stage2 :: [Token] -> Parakeet [Token]
stage2 [] = return []
stage2 tokens = do
  let (b, rs) = splitToken (head tokens)
  substitute False b rs [] (tail tokens)

internalError :: String
internalError = "[internal error] except an vowel romaji but not found"
