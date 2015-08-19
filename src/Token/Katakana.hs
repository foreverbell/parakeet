module Token.Katakana (
  fromKatakana
, isNormal
, isSmall
, isSokuon
, isKatakana
) where

import           Control.Applicative ((<$>))
import           Data.Maybe (isJust, maybeToList)
import qualified Data.Map as M
import           Prelude hiding (lookup)

import           Token.Token (Token(..), unwrapToken, isKatakanaToken)
import           Token.Misc (isChoonpu)
import           Token.Romaji (otherForms, sokuonize, longVowelize)
import           Token.Internal (kRaw)

chmap :: M.Map String String
chmap = M.fromList kRaw

fromKatakana :: Token -> [[Token]]
fromKatakana k | isKatakanaToken k = lookup (unwrapToken k)
  where
    lookup [] = []
    lookup k | isSokuon (head k) = sokuonize <$> lookup (tail k)
             | isChoonpu (last k) = longVowelize False <$> lookup (init k)
             | otherwise = map return $ concatMap otherForms $ Romaji <$> maybeToList (M.lookup k chmap)
fromKatakana _ = error "Katakana fromKatakana: not katakana token"

isNormal :: Char -> Bool
isNormal = isJust . flip M.lookup chmap . return

isSmall :: Char -> Bool
isSmall c = c `elem` ['ァ', 'ィ', 'ゥ', 'ェ', 'ォ', 'ッ', 'ャ', 'ュ', 'ョ', 'ヮ', 'ヵ', 'ヶ']
    -- [0x30a1, 0x30a3, 0x30a5, 0x30a7, 0x30a9, 0x30c3, 0x30e3, 0x30e5, 0x30e7, 0x30ee, 0x30f5, 0x30f6]

isSokuon :: Char -> Bool  -- 片仮名促音
isSokuon = (==) 'ッ'

isKatakana :: Char -> Bool
isKatakana c = isNormal c || isSmall c
