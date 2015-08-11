module JPChar (
  JPChar(..)
, hiraganaLookup
, isHiraganaSmall
, katakanaLookup
, isKatakanaSmall
, hikaLookup
, romanLookup
, isHan
, isChoonpu
, isSokuon
) where

import           Control.Monad (liftM, mplus)
import           Data.Char (ord)
import qualified Data.Map as M

data JPChar = Kanji String String String  -- han jap roman
            | Hiragana String String      -- jap roman
            | Katakana String String      -- jap roman
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

hiraganaLookup :: String -> Maybe JPChar
hiraganaLookup h = Hiragana h `liftM` M.lookup h hiraganaMap

isHiraganaSmall :: Char -> Bool
isHiraganaSmall c = (ord c) `elem` [0x3041, 0x3043, 0x3045, 0x3047, 0x3049, 0x3063, 0x3083, 0x3085, 0x3087, 0x308e, 0x3095, 0x3096] 

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

katakanaLookup :: String -> Maybe JPChar
katakanaLookup h = Katakana h `liftM` M.lookup h katakanaMap

isKatakanaSmall :: Char -> Bool
isKatakanaSmall c = (ord c) `elem` [0x30a1, 0x30a3, 0x30a5, 0x30a7, 0x30a9, 0x30c3, 0x30e3, 0x30e5, 0x30e7, 0x30ee, 0x30f5, 0x30f6]

-- * Common helpers

hikaLookup :: String -> Maybe JPChar
hikaLookup j = hiraganaLookup j `mplus` katakanaLookup j

romanMap :: M.Map String (String, String)
romanMap = M.fromList $ zipWith helper hiraganaRaw katakanaRaw
  where
    helper (h, hr) (k, kr) | hr /= kr  = error "bad data" -- never be here
                           | otherwise = (hr, (h, k))

romanLookup :: String -> Maybe (String, String)
romanLookup r = M.lookup r romanMap

isHan :: Char -> Bool      -- 漢字
isHan = (\x -> x >= 0x4e00 && x <= 0x9fbf) . ord

isChoonpu :: Char -> Bool  -- 長音符
isChoonpu c = (ord c) == 0x30fc

isSokuon :: Char -> Bool   -- 促音
isSokuon c = (ord c) `elem` [0x3063, 0x30c3]

