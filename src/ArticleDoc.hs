module ArticleDoc (
  ArticleDoc(..)
, texify
) where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Text.Printf (printf)

build :: Int -> String -> String
build f s = printf "\\%s{%s}" (fonts !! f) s
  where
    fonts = [ 
      "Huge", "huge", 
      "LARGE", "Large", "large", 
      "normalsize", "small", "footnotesize", "scriptsize", "tiny" ]

data ArticleDoc = ADList [ArticleDoc]
                | Line
                | Break
                | Lit String
                | Kanji String String String
                | Hiragana String String
                | Katakana String String
                deriving (Show)

texify :: ArticleDoc -> ByteString
texify doc = case doc of
  ADList d     -> B.concat (map texify d)
  Line         -> B.pack $ "\\\\ \n"
  Break        -> B.pack $ "\\, "
  Lit s        -> B.pack $ (build 0 s) ++ " "
  Kanji h j r  -> B.pack $ printf "\\ruby{%s(%s)}{%s} " (build 0 h) (build 2 j) (build 5 r)
  Hiragana j r -> B.pack $ printf "\\ruby{%s}{%s} " (build 0 j) (build 5 r)
  Katakana j r -> B.pack $ printf "\\ruby{%s}{%s} " (build 0 j) (build 5 r)

