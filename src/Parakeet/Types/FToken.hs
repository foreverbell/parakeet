module Parakeet.Types.FToken (
  FToken (..)
, fromToken
, concatLit
) where

import           Parakeet.Types.Lexeme (unwrap, RType)
import qualified Parakeet.Types.Token as Token
import           Parakeet.Types.Options (FuriganaFormat (..))

data FToken = Line
            | Lit String
            | Kanji String [String] [String]
            | Hiragana String [String]
            | Katakana String [String]
            deriving (Show)

fromToken :: RType a => FuriganaFormat -> Token.Token a -> FToken
fromToken _ Token.Line = Line
fromToken _ (Token.Lit l) = Lit (unwrap l)
fromToken _ (Token.Hiragana h rs) = Hiragana (unwrap h) (map unwrap rs)
fromToken _ (Token.Katakana k rs) = Katakana (unwrap k) (map unwrap rs)
fromToken furigana (Token.Kanji k hs ks rs) = Kanji (unwrap k) kana romaji
  where
    romaji = map unwrap rs
    kana = case furigana of
             InKatakana -> map unwrap ks
             InHiragana -> map unwrap hs
fromToken _ (Token.AlphaNum a Nothing) = Lit (unwrap a)
fromToken furigana (Token.AlphaNum a (Just (hs, ks, rs))) = Kanji (unwrap a) kana romaji
  where
    romaji = map unwrap rs
    kana = case furigana of
             InKatakana -> map unwrap ks
             InHiragana -> map unwrap hs

concatLit :: [FToken] -> [FToken]
concatLit ts = reverse $ go [] ts
  where
    go r [] = r
    go r xs@(Lit _:_) = go (Lit (concatMap fromLit lits) : r) rest 
      where
        fromLit (Lit l) = l
        fromLit _       = undefined
        isLit (Lit _) = True
        isLit _       = False
        (lits, rest) = span isLit xs
    go r (x:xs) = go (x:r) xs
