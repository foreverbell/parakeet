module JPToken (
  JPToken(..)
, hiraganaLookup
, isHiraganaNormal
, isHiraganaSmall
, isHiraganaSokuon
, isHiragana
, katakanaLookup
, isKatakanaNormal
, isKatakanaSmall
, isKatakanaSokuon
, isKatakana
, hikaLookup
, romajiLookup
, romajiGeminate
, romajiLVowel
, isKanji
, isChoonpu
) where

import           Control.Monad (liftM, mplus)
import           Data.Char (ord)
import           Data.Maybe (isJust)
import qualified Data.Map as M

data JPToken = Kanji String 
             | Hiragana String
             | Katakana String
             | Romaji String
             | Lit String
             deriving (Show, Eq)

-- * Hiraganas

hiraganaRaw :: [(String, String)]
hiraganaRaw = [
  ("あ", "a"), ("い", "i"), ("う", "u"), ("え", "e"), ("お", "o"),
  ("か", "ka"), ("き", "ki"), ("く", "ku"), ("け", "ke"), ("こ", "ko"),
  ("が", "ga"), ("ぎ", "gi"), ("ぐ", "gu"), ("げ", "ge"), ("ご", "go"),
  ("さ", "sa"), ("し", "shi"), ("す", "su"), ("せ", "se"), ("そ", "so"),
  ("ざ", "za"), ("じ", "ji"), ("ず", "zu"), ("ぜ", "ze"), ("ぞ", "zo"),
  ("た", "ta"), ("ち", "chi"), ("つ", "tsu"), ("て", "te"), ("と", "to"),
  ("だ", "da"), ("ぢ", "ji"), ("づ", "zu"), ("で", "de"), ("ど", "do"),
  ("な", "na"), ("に", "ni"), ("ぬ", "nu"), ("ね", "ne"), ("の", "no"),
  ("は", "ha"), ("ひ", "hi"), ("ふ", "fu"), ("へ", "he"), ("ほ", "ho"),
  ("ば", "ba"), ("び", "bi"), ("ぶ", "bu"), ("べ", "be"), ("ぼ", "bo"),
  ("ぱ", "pa"), ("ぴ", "pi"), ("ぷ", "pu"), ("ぺ", "pe"), ("ぽ", "po"),
  ("ま", "ma"), ("み", "mi"), ("む", "mu"), ("め", "me"), ("も", "mo"),
  ("や", "ya"), ("ゆ", "yu"), ("よ", "yo"), ("ら", "ra"), ("り", "ri"),
  ("る", "ru"), ("れ", "re"), ("ろ", "ro"), ("わ", "wa"), ("を", "wo"),
  ("ん", "n"), ("きゃ", "kya"), ("きゅ", "kyu"), ("きょ", "kyo"), ("ぎゃ", "gya"),
  ("ぎゅ", "gyu"), ("ぎょ", "gyo"), ("しゃ", "sha"), ("しゅ", "shu"), ("しょ", "sho"),
  ("じゃ", "ja"), ("じゅ", "ju"), ("じょ", "jo"), ("ちゃ", "cha"), ("ちゅ", "chu"),
  ("ちょ", "cho"), ("にゃ", "nya"), ("にゅ", "nyu"), ("にょ", "nyo"), ("ひゃ", "hya"),
  ("ひゅ", "hyu"), ("ひょ", "hyo"), ("びゃ", "bya"), ("びゅ", "byu"), ("びょ", "byo"),
  ("ぴゃ", "pya"), ("ぴゅ", "pyu"), ("ぴょ", "pyo"), ("みゃ", "mya"), ("みゅ", "myu"),
  ("みょ", "myo"), ("りゃ", "rya"), ("りゅ", "ryu"), ("りょ", "ryo") ]

hiraganaMap :: M.Map String String
hiraganaMap = M.fromList hiraganaRaw

hiraganaLookup :: String -> Maybe JPToken
hiraganaLookup [] = Nothing
hiraganaLookup h | isHiraganaSokuon (head h) = romajiGeminate `liftM` hiraganaLookup (tail h)
                 | otherwise                 = Romaji `liftM` M.lookup h hiraganaMap

isHiraganaNormal :: Char -> Bool
isHiraganaNormal = isJust . hiraganaLookup . return

isHiraganaSmall :: Char -> Bool
isHiraganaSmall c = c `elem` ['ぁ', 'ぃ', 'ぅ', 'ぇ', 'ぉ', 'っ', 'ゃ', 'ゅ', 'ょ', 'ゎ']
    -- [0x3041, 0x3043, 0x3045, 0x3047, 0x3049, 0x3063, 0x3083, 0x3085, 0x3087, 0x308e, 0x3095, 0x3096] 
    -- last two (3095, 3096) aren't commonly used in modern Japanese

isHiraganaSokuon :: Char -> Bool  -- 平仮名促音
isHiraganaSokuon = (==) 'っ'

isHiragana :: Char -> Bool
isHiragana c = (isHiraganaNormal c) || (isHiraganaSmall c)

-- * Katahanas

katakanaRaw :: [(String, String)]
katakanaRaw = [
  ("ア", "a"), ("イ", "i"), ("ウ", "u"), ("エ", "e"), ("オ", "o"),
  ("カ", "ka"), ("キ", "ki"), ("ク", "ku"), ("ケ", "ke"), ("コ", "ko"),
  ("ガ", "ga"), ("ギ", "gi"), ("グ", "gu"), ("ゲ", "ge"), ("ゴ", "go"),
  ("サ", "sa"), ("シ", "shi"), ("ス", "su"), ("セ", "se"), ("ソ", "so"),
  ("ザ", "za"), ("ジ", "ji"), ("ズ", "zu"), ("ゼ", "ze"), ("ゾ", "zo"),
  ("タ", "ta"), ("チ", "chi"), ("ツ", "tsu"), ("テ", "te"), ("ト", "to"),
  ("ダ", "da"), ("ヂ", "ji"), ("ヅ", "zu"), ("デ", "de"), ("ド", "do"),
  ("ナ", "na"), ("ニ", "ni"), ("ヌ", "nu"), ("ネ", "ne"), ("ノ", "no"),
  ("ハ", "ha"), ("ヒ", "hi"), ("フ", "fu"), ("ヘ", "he"), ("ホ", "ho"),
  ("バ", "ba"), ("ビ", "bi"), ("ブ", "bu"), ("ベ", "be"), ("ボ", "bo"),
  ("パ", "pa"), ("ピ", "pi"), ("プ", "pu"), ("ペ", "pe"), ("ポ", "po"),
  ("マ", "ma"), ("ミ", "mi"), ("ム", "mu"), ("メ", "me"), ("モ", "mo"),
  ("ヤ", "ya"), ("ユ", "yu"), ("ヨ", "yo"), ("ラ", "ra"), ("リ", "ri"),
  ("ル", "ru"), ("レ", "re"), ("ロ", "ro"), ("ワ", "wa"), ("ヲ", "wo"),
  ("ン", "n"), ("キャ", "kya"), ("キュ", "kyu"), ("キョ", "kyo"), ("ギャ", "gya"),
  ("ギュ", "gyu"), ("ギョ", "gyo"), ("シャ", "sha"), ("シュ", "shu"), ("ショ", "sho"),
  ("ジャ", "ja"), ("ジュ", "ju"), ("ジョ", "jo"), ("チャ", "cha"), ("チュ", "chu"),
  ("チョ", "cho"), ("ニャ", "nya"), ("ニュ", "nyu"), ("ニョ", "nyo"), ("ヒャ", "hya"),
  ("ヒュ", "hyu"), ("ヒョ", "hyo"), ("ビャ", "bya"), ("ビュ", "byu"), ("ビョ", "byo"),
  ("ピャ", "pya"), ("ピュ", "pyu"), ("ピョ", "pyo"), ("ミャ", "mya"), ("ミュ", "myu"),
  ("ミョ", "myo"), ("リャ", "rya"), ("リュ", "ryu"), ("リョ", "ryo") ]

katakanaMap :: M.Map String String
katakanaMap = M.fromList katakanaRaw

katakanaLookup :: String -> Maybe JPToken
katakanaLookup [] = Nothing
katakanaLookup k | isKatakanaSokuon (head k) = romajiGeminate `liftM` katakanaLookup (tail k)
                 | isChoonpu (last k)        = romajiLVowel `liftM` katakanaLookup (init k)
                 | otherwise                 = Romaji `liftM` M.lookup k katakanaMap

isKatakanaNormal :: Char -> Bool
isKatakanaNormal = isJust . katakanaLookup . return

isKatakanaSmall :: Char -> Bool
isKatakanaSmall c = c `elem` ['ァ', 'ィ', 'ゥ', 'ェ', 'ォ', 'ッ', 'ャ', 'ュ', 'ョ', 'ヮ', 'ヵ', 'ヶ']
    -- [0x30a1, 0x30a3, 0x30a5, 0x30a7, 0x30a9, 0x30c3, 0x30e3, 0x30e5, 0x30e7, 0x30ee, 0x30f5, 0x30f6]

isKatakanaSokuon :: Char -> Bool  -- 片仮名促音
isKatakanaSokuon = (==) 'ッ'

isKatakana :: Char -> Bool
isKatakana c = (isKatakanaNormal c) || (isKatakanaSmall c)

-- * Common helpers

hikaLookup :: String -> Maybe JPToken
hikaLookup j = hiraganaLookup j `mplus` katakanaLookup j

romajiMap :: M.Map String (String, String)
romajiMap = M.fromList $ zipWith helper hiraganaRaw katakanaRaw
  where
    helper (h, hr) (k, kr) | hr /= kr  = error "bad data" -- never be here
                           | otherwise = (hr, (h, k))

romajiLookup :: String -> Maybe (JPToken, JPToken)
romajiLookup r = (\(h, k) -> return (Hiragana h, Katakana k)) =<< (M.lookup r romajiMap)

-- chi -> tchi, ka -> kka
romajiGeminate :: JPToken -> JPToken
romajiGeminate (Romaji s@('c':'h':_)) = Romaji $ 't':s
romajiGeminate (Romaji s@(c:_))       = Romaji $ c:s
romajiGeminate _                      = error "romaji geminate: not romaji"

romajiLVowel :: JPToken -> JPToken
romajiLVowel (Romaji s) = Romaji $ s ++ [t]
  where t = last s
romajiLVowel _          = error "romaji long vowel: not romaji"

isKanji :: Char -> Bool    -- 漢字
isKanji = (\x -> x >= 0x4e00 && x <= 0x9fbf) . ord

isChoonpu :: Char -> Bool  -- 長音符
isChoonpu = (==) 'ー'
