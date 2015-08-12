module Token.Token (
  Token(..)
, unwrapToken
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
           deriving (Show, Eq)

unwrapToken :: Token -> String
unwrapToken (Kanji k) = k
unwrapToken (Hiragana h) = h
unwrapToken (Katakana k) = k
unwrapToken (Romaji r) = r
unwrapToken (Lit l) = l

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
