module Token.Token (
  Token(..)
, TokenKana(..)
, Kanji
, Hiragana
, Katakana
, Romaji
, Lit

) where

import           Control.Applicative ((<$>))

import qualified Token.Compound as C

data Kanji = Kanji String deriving (Show, Eq, Ord)
data Hiragana = Hiragana String deriving (Show, Eq, Ord)
data Katakana = Katakana String deriving (Show, Eq, Ord)
data Romaji = Romaji String deriving (Show, Eq, Ord)
data Lit = Lit String deriving (Show, Eq, Ord)

class Token t where
  unwrap :: t -> String
  wrap   :: String -> t
  (<**>) :: (String -> String) -> t -> t
  f <**> t = wrap $ f (unwrap t)
  (<$$>) :: (String -> [String]) -> t -> [t] 
  f <$$> t = wrap <$> f (unwrap t)

class (Token k) => TokenKana k where
  buildCompound :: k -> [Romaji] -> C.Compound
  toRomaji :: k -> [[Romaji]] 
  fromNRomaji :: [Romaji] -> Maybe [k]

instance Token Kanji where
  unwrap (Kanji t) = t
  wrap = Kanji

instance Token Hiragana where
  unwrap (Hiragana t) = t
  wrap = Hiragana

instance Token Katakana where
  unwrap (Katakana t) = t
  wrap = Katakana

instance Token Romaji where
  unwrap (Romaji t) = t
  wrap = Romaji

instance Token Lit where
  unwrap (Lit t) = t
  wrap = Lit
