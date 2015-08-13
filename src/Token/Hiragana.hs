module Token.Hiragana (
  fromHiragana
, toHiragana
, isNormal
, isSmall
, isSokuon
, isHiragana
) where

import           Control.Applicative ((<$>))
import           Control.Monad (liftM, guard, mplus)
import           Data.Maybe (isJust, maybeToList)
import qualified Data.Map as M

import           Token.Token (Token(..), unwrapToken, isHiraganaToken, isRomajiToken)
import           Token.Romaji (many, sokuonize, isSyllabicN, fromRomaji)
import           Token.Internal (hRaw)

chmap :: M.Map String String
chmap = M.fromList hRaw

fromHiragana :: Token -> [Token]
fromHiragana h | isHiraganaToken h = lookup (unwrapToken h)
  where
    lookup [] = []
    lookup h@(x:xs) | isSokuon x = sokuonize <$> lookup xs
                    | otherwise  = concatMap many $ Romaji <$> maybeToList (M.lookup h chmap)
fromHiragana _ = error "Hiragana fromHiragana: not hiragana token"

-- * assert already normalized
toHiragana :: [Token] -> Maybe [Token]
toHiragana h = if and (map isRomajiToken h)
  then sequence $ convert h
  else error "Hiragana toHiragana: not romaji token"
  where
    convert :: [Token] -> [Maybe Token]
    convert [] = []
    convert (x:xs) = if isSyllabicN x
                       then (Just (Hiragana "ん")) : convert xs
                       else (lookupNormal x `mplus` lookupChoonpu x xs) : convert xs
                         where
                           lookupNormal x = fst `liftM` fromRomaji x
                           lookupChoonpu x (y:_) = do
                             guard $ length x' == 1
                             guard $ not $ null y'
                             guard $ head x' == head y'
                             return $ Hiragana "っ"
                             where x' = unwrapToken x
                                   y' = unwrapToken y
                           lookupChoonpu _ _ = Nothing

isNormal :: Char -> Bool
isNormal = isJust . flip M.lookup chmap . return

isSmall :: Char -> Bool
isSmall c = c `elem` ['ぁ', 'ぃ', 'ぅ', 'ぇ', 'ぉ', 'っ', 'ゃ', 'ゅ', 'ょ', 'ゎ']
    -- [0x3041, 0x3043, 0x3045, 0x3047, 0x3049, 0x3063, 0x3083, 0x3085, 0x3087, 0x308e, 0x3095, 0x3096] 
    -- last two (3095, 3096) aren't commonly used in modern Japanese (not displayable).

isSokuon :: Char -> Bool  -- 平仮名促音
isSokuon = (==) 'っ'

isHiragana :: Char -> Bool
isHiragana c = (isNormal c) || (isSmall c)
