module Parakeet.Parser.Stage2 (
  stage2
) where

import           Control.Monad (when)
import           Control.Monad.Parakeet (Parakeet, throw)

import           Parakeet.Types.Token
import qualified Parakeet.Types.Lexeme as L
import           Parakeet.Linguistics.Romaji (longVowelize1WithMacron)

splitToken :: Token L.Single -> ([L.Romaji L.Bundle] -> Token L.Bundle, [L.Romaji L.Single])
splitToken token = case token of
  Line -> (const Line, [])
  Lit s -> (const (Lit s), [])
  AlphaNum a Nothing -> (const (AlphaNum a Nothing), [])
  AlphaNum a (Just (h, k, rs)) -> (\rs -> AlphaNum a $ Just (h, k, rs), rs)
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
substitute :: Bool -> ([L.Romaji L.Bundle] -> Token L.Bundle) -> [L.Romaji L.Single] -> [L.Romaji L.Bundle] -> [Token L.Single] -> Parakeet [Token L.Bundle]
substitute False b [] d [] = return [b (reverse d)] 

substitute False b (c:r) d r0 = if L.isRLV c
  then substitute True b r (longVowelize1WithMacron c:d) r0
  else substitute False b r (L.toRB c:d) r0

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

stage2 :: [Token L.Single] -> Parakeet [Token L.Bundle]
stage2 [] = return []
stage2 tokens = do
  let (b, rs) = splitToken (head tokens)
  substitute False b rs [] (tail tokens)

internalError :: String
internalError = "[internal error] except an vowel romaji but not found"
