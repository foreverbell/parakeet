module Element (
  Element(..)
, texify
) where

import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Text.Printf (printf)
import           Data.List (intercalate)

import           Eval (Eval)

build :: Int -> String -> String
build f = printf "\\%s{%s}" (fonts !! f)
  where fonts = [ "Huge"
                , "huge"
                , "LARGE"
                , "Large"
                , "large"
                , "normalsize"
                , "small"
                , "footnotesize"
                , "scriptsize"
                , "tiny" 
                ]

data Element = Line
             | Break
             | Lit String
             | Kanji String [String] [String]  -- kanji, hiragana, romaji
             | Hiragana String [String]        -- hiragana, romaji
             | Katakana String [String]        -- katakana, romaji
             deriving (Show)

texify :: [Element] -> Eval Text
texify ds = return $ T.concat $ map singleTexify ds
  where
    mainFont = 4
    rubyFont = 6
    romajiFont = 5
    singleTexify d = case d of
      Line         -> T.pack $ " \\\\ \n"
      Break        -> T.pack $ "\\, "
      Lit s        -> T.pack $ build mainFont s ++ " "
      Kanji k h r  -> T.pack $ printf "\\ruby{%s%s}{%s} " (build mainFont k) (build rubyFont ("(" ++ concat h ++ ")")) (build romajiFont (intercalate " " r))
      Hiragana h r -> T.pack $ printf "\\ruby{%s}{%s} " (build mainFont h) (build romajiFont (intercalate " " r))
      Katakana k r -> T.pack $ printf "\\ruby{%s}{%s} " (build mainFont k) (build romajiFont (intercalate " " r))

