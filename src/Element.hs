module Element (
  Element(..)
, texify
) where

import           Control.Applicative ((<$>))
import           Control.Monad.Reader (asks)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Text.Printf (printf)
import           Data.List (intercalate)

import           Eval (Eval)
import           Options (Options(..))

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
texify ds = T.concat <$> mapM singleTexify ds
  where
    mainFont = 4
    rubyFont = 6
    romajiFont = 5
    singleTexify d = case d of
      Line         -> return $ T.pack $ " \\\\ \n"
      Break        -> do
        showBreak <- asks optShowBreak
        return $ if showBreak
          then T.pack $ "\\, "
          else T.empty
      Lit s        -> return $ T.pack $ build mainFont s ++ " "
      Kanji k h r  -> return $ T.pack $ printf "\\ruby{%s%s}{%s} " (build mainFont k) (build rubyFont ("(" ++ concat h ++ ")")) (build romajiFont (intercalate " " r))
      Hiragana h r -> return $ T.pack $ printf "\\ruby{%s}{%s} " (build mainFont h) (build romajiFont (intercalate " " r))
      Katakana k r -> return $ T.pack $ printf "\\ruby{%s}{%s} " (build mainFont k) (build romajiFont (intercalate " " r))

