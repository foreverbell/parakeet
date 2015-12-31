{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Parakeet.Types.Lexeme (
  Lexeme(..)
, LexemeKana(..)
, Lit
, Kanji
, Hiragana
, Katakana
, Romaji
, isRLV
, toRLV
, concatRomajis
, Bundle
, Single
) where

import Control.Monad.Choice (Choice)

data Bundle
data Single
-- | Phantom type `a` to distinguish bundle and single romaji.
data Romaji a = Romaji String 
              | RomajiLV String -- romaji with long vowel
              deriving (Show)

newtype Lit = Lit String deriving (Show)
newtype Kanji = Kanji String deriving (Show)
newtype Hiragana = Hiragana String deriving (Show)
newtype Katakana = Katakana String deriving (Show)

instance Eq (Romaji a) where
  a == b = unwrap a == unwrap b

instance Ord (Romaji a) where
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
  -- | toRomaji k: sokuon ++ body ++ choonpu (katakana only) / itermark 
  toRomaji :: k -> Choice [Romaji Single] 
  fromRomaji :: [Romaji Single] -> [Maybe k] 

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

instance Lexeme (Romaji a) where
  unwrap (Romaji t) = t
  unwrap (RomajiLV t) = t
  wrap = Romaji

isRLV :: Romaji a -> Bool
isRLV (RomajiLV _) = True
isRLV _            = False

toRLV :: Romaji a -> Romaji a
toRLV = RomajiLV . unwrap

concatRomajis :: [Romaji Single] -> Romaji Bundle
concatRomajis = wrap . concatMap unwrap
