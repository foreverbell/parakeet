{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Linguistics.Lexeme (
  Lexeme(..)
, LexemeKana(..)
, Kanji
, Hiragana
, Katakana
, Romaji
, Lit
, Separator
) where

import           Monad.Choice (Choice)
import qualified Parser.Token as Token

newtype Kanji = Kanji String deriving (Show, Eq, Ord)
newtype Hiragana = Hiragana String deriving (Show, Eq, Ord)
newtype Katakana = Katakana String deriving (Show, Eq, Ord)
newtype Romaji = Romaji String deriving (Show, Eq, Ord)
newtype Lit = Lit String deriving (Show, Eq, Ord)
-- TODO: Dollar type seems a bit special
data Separator = Separator deriving (Show, Eq, Ord)

infixl 4 <**>, <$$>

class Lexeme t where
  unwrap :: t -> String
  wrap   :: String -> t
  (<**>) :: (String -> String) -> t -> t
  f <**> t = wrap $ f (unwrap t)
  (<$$>) :: Functor f => (String -> f String) -> t -> f t 
  f <$$> t = wrap <$> f (unwrap t)

class (Lexeme k) => LexemeKana k where
  buildToken :: k -> [Romaji] -> Token.Token
  toRomaji :: k -> Choice [Romaji] 
  fromRomaji :: [Romaji] -> [Choice k] -- call Linguistics.Romaji.cut first 

instance Lexeme Kanji where
  unwrap (Kanji t) = t
  wrap = Kanji

instance Lexeme Hiragana where
  unwrap (Hiragana t) = t
  wrap = Hiragana

instance Lexeme Katakana where
  unwrap (Katakana t) = t
  wrap = Katakana

instance Lexeme Romaji where
  unwrap (Romaji t) = t
  wrap = Romaji

instance Lexeme Lit where
  unwrap (Lit t) = t
  wrap = Lit

instance Lexeme Separator where
  unwrap = const []
  wrap = const Separator

instance Lexeme t => Monoid t where
  mempty = wrap []
  mappend a b = wrap $ unwrap a ++ unwrap b
