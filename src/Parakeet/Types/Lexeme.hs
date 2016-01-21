{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

module Parakeet.Types.Lexeme (
  Lexeme(..)
, LexemeKana(..)
, SomeLexeme(..)
, fromLexeme
, Lit
, AlphaNum
, Kanji
, Hiragana
, Katakana
, Romaji
, isRLV
, toRLV
, toRB
, toRS
, concatR
, appendR
, Bundle
, Single
, RType
) where

import Control.Monad.Choice (Choice)
import Data.Typeable (Typeable, cast)

data Bundle deriving (Typeable)
data Single deriving (Typeable)

class Typeable a => RType a
instance RType Bundle
instance RType Single

-- | Phantom type `a` to distinguish bundle and single romaji.
-- Based on ideas in /https://wiki.haskell.org/Phantom_type/.
data RType a => Romaji a = Romaji String 
                         | RomajiLV String -- romaji with long vowel
                         deriving (Show, Typeable)

newtype Lit = Lit String deriving (Show, Typeable)
newtype AlphaNum = AlphaNum String deriving (Show, Typeable)
newtype Kanji = Kanji String deriving (Show, Typeable)
newtype Hiragana = Hiragana String deriving (Show, Typeable)
newtype Katakana = Katakana String deriving (Show, Typeable)

instance RType a => Eq (Romaji a) where
  a == b = unwrap a == unwrap b

instance RType a => Ord (Romaji a) where
  a `compare` b = unwrap a `compare` unwrap b
  
infixl 4 <**>, <$$>

class Typeable t => Lexeme t where
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

-- | Based on ideas in /An Extensible Dynamically-Typed Hierarchy of
-- Exceptions/, Simon Marlow, 2006. 
data SomeLexeme = forall k. Lexeme k => SomeLexeme k deriving (Typeable)

fromLexeme :: Lexeme k => SomeLexeme -> Maybe k
fromLexeme (SomeLexeme k) = cast k

instance Lexeme Lit where
  unwrap (Lit t) = t
  wrap = Lit

instance Lexeme AlphaNum where
  unwrap (AlphaNum t) = t
  wrap = AlphaNum 

instance Lexeme Kanji where
  unwrap (Kanji t) = t
  wrap = Kanji

instance Lexeme Hiragana where
  unwrap (Hiragana t) = t
  wrap = Hiragana

instance Lexeme Katakana where
  unwrap (Katakana t) = t
  wrap = Katakana

instance RType a => Lexeme (Romaji a) where
  unwrap (Romaji t) = t
  unwrap (RomajiLV t) = t
  wrap = Romaji

isRLV :: RType a => Romaji a -> Bool
isRLV (RomajiLV _) = True
isRLV _            = False

toRLV :: RType a => Romaji a -> Romaji a
toRLV = RomajiLV . unwrap

toRB :: RType a => Romaji a -> Romaji Bundle
toRB (Romaji r)   = Romaji r
toRB (RomajiLV r) = RomajiLV r

toRS :: RType a => Romaji a -> Romaji Single
toRS (Romaji r)   = Romaji r
toRS (RomajiLV r) = RomajiLV r

concatR :: RType a => [Romaji a] -> Romaji Bundle
concatR = wrap . mconcat . map unwrap

appendR :: (RType a, RType b) => Romaji a -> Romaji b -> Romaji Bundle
appendR a b = wrap $ unwrap a `mappend` unwrap b
