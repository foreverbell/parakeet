module TexElem (
  TexElem(..)
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

data TexElem = Line
             | Break
             | Lit String
             | Kanji String String String  -- kanji, hiragana(?), romaji
             | Hiragana String String      -- hiragana, romaji
             | Katakana String String      -- katakana, romaji
             deriving (Show)

texify :: TexElem -> ByteString
texify doc = case doc of
  Line         -> B.pack $ "\\\\ \n"
  Break        -> B.pack $ "\\, "
  Lit s        -> B.pack $ (build 0 s) ++ " "
  Kanji k h r  -> B.pack $ printf "\\ruby{%s(%s)}{%s} " (build 0 k) (build 2 h) (build 5 r)
  Hiragana h r -> B.pack $ printf "\\ruby{%s}{%s} " (build 0 h) (build 5 r)
  Katakana k r -> B.pack $ printf "\\ruby{%s}{%s} " (build 0 k) (build 5 r)
