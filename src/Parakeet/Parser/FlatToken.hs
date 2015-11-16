module Parakeet.Parser.FlatToken (
  FlatToken(..)
, flatten
) where

import           Control.Monad.Reader (asks)
import           Control.Monad.Parakeet (Parakeet)

import qualified Parakeet.Parser.Token as Token
import qualified Parakeet.Linguistics.Lexeme as L
import           Parakeet.Linguistics.Hiragana ()
import           Parakeet.Linguistics.Katakana ()
import           Parakeet.Options (Options(..), FuriganaFormat(..))

data FlatToken
    = Line
    | Break
    | Lit String
    | Kanji String [String] [String]
    | Hiragana String [String]
    | Katakana String [String]
    deriving (Show)

flatten :: Token.Token -> Parakeet FlatToken
flatten Token.Line = return Line
flatten Token.Break = return Break
flatten (Token.Lit l) = return $ Lit (L.unwrap l)
flatten (Token.Kanji k hs ks r) = do
  let romaji = map L.unwrap r
  furigana <- asks optFurigana
  let kana = case furigana of
        InKatakana -> map L.unwrap ks
        InHiragana -> map L.unwrap hs
  return $ Kanji (L.unwrap k) kana romaji
flatten (Token.Hiragana h r) = return $ Hiragana (L.unwrap h) (map L.unwrap r)
flatten (Token.Katakana k r) = return $ Katakana (L.unwrap k) (map L.unwrap r)

