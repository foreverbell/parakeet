module Data.Char.Fuzzy (
  canonicalOrd
, fuzzyEq
, fuzzyAlphaNum
) where

import           Data.Char (ord, chr, isAlphaNum)
import           Prelude hiding (elem)
import qualified Data.List as L

inside :: Int -> Int -> Int -> Bool
inside l r x = x >= l && x <= r

eq :: Int -> Int -> Bool
eq = (==)

elem :: [Int] -> Int -> Bool
elem = flip L.elem

{-

and :: (Int -> Bool) -> (Int -> Bool) -> (Int -> Bool)
and p q c = p c && q c

or :: (Int -> Bool) -> (Int -> Bool) -> (Int -> Bool)
or p q c = p c || q c

-}

infix 0 ==>
(==>) a b = (a, b)

-- | Rules for characters transformation. If two rules overlap, always pick the first rule.
rules :: [(Int -> Bool, Int -> Int)]
rules = [ elem [0x60, 0xff40]   ==> const 0x27              -- ` -> '
        , inside 0xff01 0xff5e  ==> \c -> c - 0xff01 + 0x21 -- Full width char to half width
        , eq 0x3001             ==> const 0x2c              -- Full comma
        , eq 0x3002             ==> const 0x2e              -- Full stop
        , elem [0x300c, 0xff62] ==> const 0x27              -- Single quotation mark
        , elem [0x300d, 0xff63] ==> const 0x27
        , elem [0x300e, 0x201c] ==> const 0x22              -- Double quotation mark
        , elem [0x300f, 0x201d] ==> const 0x22
        ]

canonicalOrd :: Char -> Int
canonicalOrd c = case L.find (\(p, _) -> p x) rules of
  Nothing     -> x
  Just (_, f) -> f x
  where x = ord c

fuzzyEq :: Char -> Char -> Bool
fuzzyEq a b  = (==) (canonicalOrd a) (canonicalOrd b) 

fuzzyAlphaNum :: Char -> Bool
fuzzyAlphaNum c | inside 0xff01 0xff5e x = fuzzyAlphaNum (chr (x - 0xff01 + 0x21))
                | x < 256 = isAlphaNum c
                | otherwise = False
  where x = ord c
