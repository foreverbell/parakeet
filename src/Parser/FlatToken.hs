module Parser.FlatToken (
  FlatToken(..)
, flatten
) where

import           Control.Monad.Reader (asks)

import qualified Parser.Token as Token
import qualified Linguistics.Lexeme as L
import           Linguistics.Hiragana ()
import           Linguistics.Katakana ()
import           Options (Options(..), FuriganaFormat(..))
import           Monad.Parakeet (Parakeet)

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
        _          -> map L.unwrap hs
  return $ Kanji (L.unwrap k) kana romaji
flatten (Token.Hiragana h r) = return $ Hiragana (L.unwrap h) (map L.unwrap r)
flatten (Token.Katakana k r) = return $ Katakana (L.unwrap k) (map L.unwrap r)

