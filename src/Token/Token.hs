{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Token.Token (
  Token(..)
, TokenKana(..)
, Kanji
, Hiragana
, Katakana
, Romaji
, Lit
, Separator
) where

import           Monad.Choice (Choice)
import qualified Token.Compound as C

newtype Kanji = Kanji String deriving (Show, Eq, Ord)
newtype Hiragana = Hiragana String deriving (Show, Eq, Ord)
newtype Katakana = Katakana String deriving (Show, Eq, Ord)
newtype Romaji = Romaji String deriving (Show, Eq, Ord)
newtype Lit = Lit String deriving (Show, Eq, Ord)
-- TODO: Dollar type seems a bit special
data Separator = Separator deriving (Show, Eq, Ord)

infixl 4 <**>, <$$>

class Token t where
  unwrap :: t -> String
  wrap   :: String -> t
  (<**>) :: (String -> String) -> t -> t
  f <**> t = wrap $ f (unwrap t)
  (<$$>) :: Functor f => (String -> f String) -> t -> f t 
  f <$$> t = wrap <$> f (unwrap t)

class (Token k) => TokenKana k where
  buildCompound :: k -> [Romaji] -> C.Compound
  toRomaji :: k -> Choice [Romaji] 
  fromRomaji :: [Romaji] -> [Choice k] -- call Token.Romaji.cut first 

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

instance Token Separator where
  unwrap = const []
  wrap = const Separator

instance Token t => Monoid t where
  mempty = wrap []
  mappend a b = wrap $ unwrap a ++ unwrap b
