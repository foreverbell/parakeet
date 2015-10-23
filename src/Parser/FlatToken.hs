module Parser.FlatToken (
  FlatToken(..)
, flatten
) where

import           Control.Monad.Reader (asks)

import qualified Parser.Token as Token
import           Linguistics.Lexeme (unwrap)
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
flatten (Token.Lit l) = return $ Lit (unwrap l)
flatten (Token.Kanji j h k r) = do
  furigana <- asks optFurigana
  let kana = case furigana of
        InKatakana -> map unwrap k
        _          -> map unwrap h
  let romaji = map unwrap r
  return $ Kanji (unwrap j) kana romaji
flatten (Token.Hiragana h r) = return $ Hiragana (unwrap h) (map unwrap r)
flatten (Token.Katakana k r) = return $ Katakana (unwrap k) (map unwrap r)

