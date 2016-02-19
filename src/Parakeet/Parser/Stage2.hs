module Parakeet.Parser.Stage2 (
  stage2
) where

import           Control.Monad (when)
import           Control.Monad.Parakeet (Parakeet, toException, SomeException, InternalError (..), throw)

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

convertMacron :: Bool                                    -- ^ If the next romaji is the part of a long vowel
              -> ([L.Romaji L.Bundle] -> Token L.Bundle) -- ^ Token builder
              -> [L.Romaji L.Single]                     -- ^ Romajis wait to be process for the current token
              -> [L.Romaji L.Bundle]                     -- ^ Processed romajis of the current token
              -> [Token L.Single]                        -- ^ Remaining tokens wait to be processed
              -> Parakeet [Token L.Bundle]
convertMacron False b [] d [] = return [b (reverse d)] 

convertMacron False b (c:r) d r0 = if L.isRLV c
  then convertMacron True b r (longVowelize1WithMacron c:d) r0
  else convertMacron False b r (L.toRB c:d) r0

convertMacron False b [] d r0 = do
  let (b', rs) = splitToken (head r0)
  n <- convertMacron False b' rs [] (tail r0)
  return $ b (reverse d) : n

convertMacron True _ [] _ [] = throw internalError

convertMacron True b (_:r) d r0 = convertMacron False b r d r0

convertMacron True b [] d r0 = do
  let (b', rs) = splitToken (head r0)
  when (null rs) $ throw internalError
  n <- convertMacron False b' (tail rs) [] (tail r0)
  return $ b (reverse d) : n

stage2 :: [Token L.Single] -> Parakeet [Token L.Bundle]
stage2 [] = return []
stage2 tokens = do
  let (b, rs) = splitToken (head tokens)
  convertMacron False b rs [] (tail tokens)

internalError :: SomeException
internalError = toException $ InternalError "except an vowel romaji but not found"
