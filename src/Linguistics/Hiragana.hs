module Linguistics.Hiragana (
  isNormal
, isSmall
, isSokuon
, isHiragana
) where

import           Control.Monad (guard, msum, mzero, join)
import           Data.Maybe (isJust)
import qualified Data.Map as M

import qualified Parser.Token as Token
import           Linguistics.Lexeme (LexemeKana(..), wrap, unwrap, Hiragana)
import           Linguistics.Romaji (otherForms, sokuonize, isSyllabicN, toKana)
import           Linguistics.Internal (hRaw)
import           Monad.Choice (fromMaybe)

chmap :: M.Map String String
chmap = M.fromList hRaw

instance LexemeKana Hiragana where
  buildToken k r = Token.Hiragana (unwrap k) (map unwrap r)

  toRomaji h = lookup (unwrap h)
    where
      lookup [] = mzero
      lookup h@(x:xs) | isSokuon x = sokuonize <$> lookup xs
                      | otherwise  = return <$> join (otherForms . wrap <$> fromMaybe (M.lookup h chmap))
  
  fromRomaji = convert
    where
      convert [] = []
      convert (x:xs) = msum (map (\f -> f x) [checkSyllabicN, lookupNormal, checkChoonpu]) : convert xs
        where
          checkSyllabicN x = do
            guard $ isSyllabicN x
            fst $ toKana (wrap "n")
          lookupNormal x = fst $ toKana x
          checkChoonpu x = do
            guard $ length x' == 1
            return $ wrap "っ"
            where x' = unwrap x

isNormal :: Char -> Bool
isNormal = isJust . flip M.lookup chmap . return

isSmall :: Char -> Bool
isSmall c = c `elem` ['ぁ', 'ぃ', 'ぅ', 'ぇ', 'ぉ', 'っ', 'ゃ', 'ゅ', 'ょ', 'ゎ']
    -- [0x3041, 0x3043, 0x3045, 0x3047, 0x3049, 0x3063, 0x3083, 0x3085, 0x3087, 0x308e, 0x3095, 0x3096] 
    -- last two (3095, 3096) aren't commonly used in modern Japanese.

isSokuon :: Char -> Bool
isSokuon = (==) 'っ'

isHiragana :: Char -> Bool
isHiragana c = isNormal c || isSmall c