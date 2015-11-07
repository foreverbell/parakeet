{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Linguistics.Lexeme (
  Lexeme(..)
, LexemeKana(..)
, Lit
, Kanji
, Hiragana
, Katakana
, Romaji
, isRLV
, toRLV
) where

import Monad.Choice (Choice)

newtype Lit = Lit String deriving (Show)
newtype Kanji = Kanji String deriving (Show)
newtype Hiragana = Hiragana String deriving (Show)
newtype Katakana = Katakana String deriving (Show)
data Romaji = Romaji String 
            | RomajiLV String -- romaji with long vowel
            deriving (Show)

instance Eq Romaji where
  a == b = unwrap a == unwrap b

instance Ord Romaji where
  a `compare` b = unwrap a `compare` unwrap b
  
infixl 4 <**>, <$$>

class Lexeme t where
  unwrap :: t -> String
  wrap   :: String -> t
  (<**>) :: (String -> String) -> t -> t
  f <**> t = wrap $ f (unwrap t)
  (<$$>) :: Functor f => (String -> f String) -> t -> f t 
  f <$$> t = wrap <$> f (unwrap t)

class (Lexeme k) => LexemeKana k where
  -- toRomaji k: sokuon ++ body ++ choonpu (katakana only) / itermark 
  toRomaji :: k -> Choice [Romaji] 
  fromRomaji :: [Romaji] -> [Maybe k] 

instance Lexeme Lit where
  unwrap (Lit t) = t
  wrap = Lit

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
  unwrap (RomajiLV t) = t
  wrap = Romaji

isRLV :: Romaji -> Bool
isRLV (RomajiLV _) = True
isRLV _            = False

toRLV :: Romaji -> Romaji
toRLV = RomajiLV . unwrap

instance Lexeme t => Monoid t where
  mempty = wrap []
  mappend a b = wrap $ unwrap a ++ unwrap b
