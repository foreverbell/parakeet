module Token.Token (
  Token
, unwrap
, wrap
, Kanji
, Hiragana
, Katakana
, Romaji
, Lit
, (<**>)
, (<$$>)
) where

import Control.Applicative ((<$>))

class Token t where
  unwrap :: t -> String
  wrap   :: String -> t

data Kanji = Kanji String deriving (Show, Eq, Ord)
instance Token Kanji where
  unwrap (Kanji t) = t
  wrap = Kanji

data Hiragana = Hiragana String deriving (Show, Eq, Ord)
instance Token Hiragana where
  unwrap (Hiragana t) = t
  wrap = Hiragana

data Katakana = Katakana String deriving (Show, Eq, Ord)
instance Token Katakana where
  unwrap (Katakana t) = t
  wrap = Katakana

data Romaji = Romaji String deriving (Show, Eq, Ord)
instance Token Romaji where
  unwrap (Romaji t) = t
  wrap = Romaji

data Lit = Lit String deriving (Show, Eq, Ord)
instance Token Lit where
  unwrap (Lit t) = t
  wrap = Lit

{-# INLINE (<**>) #-}
(<**>) :: Token t => (String -> String) -> t -> t
f <**> t = wrap $ f (unwrap t)

{-# INLINE (<$$>) #-}
(<$$>) :: Token t => (String -> [String]) -> t -> [t] 
f <$$> t = wrap <$> f (unwrap t)
