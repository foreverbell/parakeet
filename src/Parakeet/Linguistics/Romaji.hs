module Parakeet.Linguistics.Romaji (
  chList
, toKana
, otherForms
, dakutenize
, unDakutenize
, isSyllabicN
, normSyllabicN
, sokuonize
, unSokuonize
, longVowelize
, longVowelizeWithMacron
, longVowelize1WithMacron 
, unLongVowelize
, factor
) where

import           Data.Functor.Compose (Compose (..))
import           Data.List (sort, group)
import           Data.List.Extra (concatMapLast)
import qualified Data.Map as M
import           Data.Maybe (maybeToList, fromJust, fromMaybe, isNothing)
import           Data.Tuple (swap)
import           Control.Arrow (second)
import           Control.Monad (mzero, guard)
import           Control.Monad.Choice (Choice, fromList, toList, foremost)

import           Parakeet.Types.Lexeme (Lexeme (..), toRLV, toRS, toRB, Hiragana, Katakana, Romaji, RType, Bundle, Single)
import           Parakeet.Linguistics.Misc (isMacron, toMacron, fromMacron, isVowel)
import           Parakeet.Linguistics.RawData (hRaw, kRaw)

chList :: [Romaji Single]
chList = nub' $ concatMap (toList . otherForms . wrap . snd) $ hRaw ++ kRaw
  where nub' = map head . group . sort

buildMap :: [(String, String)] -> M.Map String (Choice String)
buildMap raw = M.fromList $ map toChoice (raw ++ raw')
  where 
    swapped = map swap raw
    toChoice (k, r) = (r, return k)
    raw' = concatMap f (tail otherFormList) 
      where 
        f (r, rs) = foldl g [] rs where
          kana = fromJust $ lookup r swapped
          g xs cur = case lookup cur swapped of
            Nothing -> (kana, cur) : xs
            _ -> xs

hMap, kMap :: M.Map String (Choice String)
hMap = buildMap hRaw
kMap = buildMap kRaw

toKana :: Romaji Single -> (Choice Hiragana, Choice Katakana)
toKana r = (lookup hMap, lookup kMap)
  where
    lookup m = case M.lookup (unwrap r) m of
      Just ch -> wrap <$> ch
      Nothing -> mzero

otherFormList :: [(String, [String])]
otherFormList = [ ("n",  ["n", "m", "nn", "n-", "n'"]) -- syllabic n
                , ("ha", ["ha", "wa"])                 -- particles mutation
                , ("he", ["he", "e"])
                , ("wo", ["wo", "o"])
                , ("di", ["di", "ji", "dji"])          -- ambiguous ji & zu
                , ("du", ["du", "zu", "dzu"])
                ]

otherFormMap :: M.Map String (Choice String)
otherFormMap = M.fromList $ map (second fromList) otherFormList

otherForms :: Romaji Single -> Choice (Romaji Single)
otherForms r = go `lfap` r
  where go r = fromMaybe (return r) (M.lookup r otherFormMap)

buildDakutenPairs :: [(Int, Int)] -> [(String, String)]
buildDakutenPairs = extend . concatMap convert 
  where
    nline = replicate 14 5 ++ [1] ++ replicate 11 3 :: [Int]
    prefix = zipWith (+) (0 : prefix) nline
    convert (l1, l2) = map (\x -> (snd (hRaw !! (offset1 + x)), snd (hRaw !! (offset2 + x)))) [0 .. count - 1]
      where 
        count = nline !! l1
        offset1 = prefix !! (l1 - 1)
        offset2 = prefix !! (l2 - 1)
    extend pairs = pairs ++ concatMap f pairs
      where f (s1, s2) = do
              x <- unwrap <$> toList (otherForms (wrap s1))
              guard $ isNothing (lookup x pairs)
              return (x, s2)

dakutenMap, unDakutenMap :: M.Map String String
dakutenMap = M.fromList $ buildDakutenPairs (dakutenPair1 ++ map swap dakutenPair3)
unDakutenMap = M.fromList $ buildDakutenPairs $ map swap (dakutenPair1 ++ dakutenPair2)

dakutenPair1, dakutenPair2, dakutenPair3 :: [(Int, Int)]
dakutenPair1 = [(1, 2), (3, 4), (5, 6), (8, 9), (15, 16), (17, 18), (21, 22)] -- (normal, dakuten)
dakutenPair2 = [(8, 10), (21, 23)] -- (normal, han-dakuten)
dakutenPair3 = [(9, 10), (22, 23)] -- (dakuten, han-dakuten)

dakutenize :: Romaji Single -> Romaji Single
dakutenize r = lookup `lap` r
  where lookup x = fromMaybe x (M.lookup x dakutenMap)

unDakutenize :: Romaji Single -> Romaji Single
unDakutenize r = lookup `lap` r
  where lookup x = fromMaybe x (M.lookup x unDakutenMap)

isSyllabicN :: Romaji Single -> Bool
isSyllabicN n = n `elem` toList (otherForms (wrap "n"))

-- | normalize syllabic n (take the first alphabet).
normSyllabicN :: Romaji Single -> Romaji Single
normSyllabicN r | isSyllabicN r = (return . head) `lap` r
                | otherwise = r

-- | sokuonize a factorized romaji, chi -> tchi, ka -> kka, a -> a.
sokuonize :: [Romaji Single] -> Choice [Romaji Single]
sokuonize [] = return []
sokuonize r = getCompose (lfap (Compose . sokuonizeInternal) (head r)) >>= \h -> return (h ++ tail r)

unSokuonize :: Romaji Bundle -> Choice (Maybe (Romaji Single), Romaji Bundle)
unSokuonize r
  | length r' <= 1 = return (Nothing, r)
  | firstOne == 't' && take 2 exceptFirst == "ch" = return (Just $ wrap [firstOne], tail `lap` r)
  | sokuonizable firstOne && head exceptFirst == firstOne = return (Just $ wrap [firstOne], tail `lap` r)
  | otherwise = return (Nothing, r)
  where
    r' = unwrap r
    firstOne = head r'
    exceptFirst = tail r'

sokuonizable :: Char -> Bool
-- | https://en.wikipedia.org/wiki/Sokuon
sokuonizable c = c `notElem` "aiueonmrwy" -- ++ "gzdbh"

sokuonizeInternal :: String -> Choice [String]
sokuonizeInternal [] = return []
sokuonizeInternal s@('c':'h':_) = fromList [["t", s], ["c", s]]
sokuonizeInternal s@(c:_) | sokuonizable c = return [[c], s]
                          | otherwise      = return [s]

-- | long vowelize a factorized romaji.
longVowelize :: [Romaji Single] -> [Romaji Single]
longVowelize [] = []
longVowelize r = concatMapLast (longVowelizeInternal False `lfap`) r

longVowelizeWithMacron :: RType a => [Romaji a] -> [Romaji Bundle]
longVowelizeWithMacron [] = []
longVowelizeWithMacron r = toRB <$> concatMapLast (longVowelizeInternal True `lfap`) r

longVowelize1WithMacron :: RType a => Romaji a -> Romaji Bundle
longVowelize1WithMacron r = toRB $ head (longVowelizeInternal True `lfap` r)

unLongVowelize :: Romaji Bundle -> Choice (Maybe (Romaji Single), Romaji Bundle)
unLongVowelize r 
  | null r' = return (Nothing, r)
  | isMacron lastOne = lastTo >>= \to -> return (Just (wrap [to]), wrap $ exceptLast ++ [lastDesugar])
  | otherwise = return (Nothing, r)
  where
    r' = unwrap r
    lastOne = last r'
    exceptLast = init r'
    lastDesugar = fromMacron lastOne
    lastTo | lastDesugar == 'o' = fromList ['u', 'o']  -- ambiguous 'ō' -> ou
           | otherwise          = return lastDesugar

longVowelizeInternal :: Bool -> String -> [String]
longVowelizeInternal _ [] = []
longVowelizeInternal m s | not (isVowel ls) = [s]
                         | m                = [concatMapLast (return . foremost . toMacron) s]
                         | otherwise        = [s, [ls]] 
  where ls = last s

-- | factorize a bundle romaji with possible sokuon & macron into different single parts, e.g. tchī -> [t, chi, i].
factor :: Romaji Bundle -> Choice [Romaji Single]
factor r = do 
  (sokuonPart, next) <- unSokuonize r
  (longVowelPart, temp) <- unLongVowelize next
  let normalized | null longVowelPart = toRS temp
                 | otherwise          = toRLV $ toRS temp
  return $ maybeToList sokuonPart ++ [normalized] ++ maybeToList longVowelPart
