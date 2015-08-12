module Token.Token (
  Token(..)
, (<$.>)
, unwrapToken
, unwrapToken'
, isKanjiToken
, isHiraganaToken
, isKatakanaToken
, isRomajiToken
, isLitToken
) where

data Token = Kanji    String
           | Hiragana String
           | Katakana String
           | Romaji   String
           | Lit      String
           deriving (Show, Eq, Ord)

(<$.>) :: (String -> String) -> Token -> Token 
f <$.> t = c (f v)
  where (v, c) = unwrapToken' t

unwrapToken' :: Token -> (String, String -> Token)
unwrapToken' (Kanji k) = (k, Kanji)
unwrapToken' (Hiragana h) = (h, Hiragana)
unwrapToken' (Katakana k) = (k, Katakana)
unwrapToken' (Romaji r) = (r, Romaji)
unwrapToken' (Lit l) = (l, Lit)

unwrapToken :: Token -> String
unwrapToken = fst . unwrapToken'

isKanjiToken :: Token -> Bool
isKanjiToken (Kanji _) = True
isKanjiToken _         = False

isHiraganaToken :: Token -> Bool
isHiraganaToken (Hiragana _) = True
isHiraganaToken _            = False

isKatakanaToken :: Token -> Bool
isKatakanaToken (Katakana _) = True
isKatakanaToken _            = False

isRomajiToken :: Token -> Bool
isRomajiToken (Romaji _) = True
isRomajiToken _          = False

isLitToken :: Token -> Bool
isLitToken (Lit _) = True
isLitToken _       = False
