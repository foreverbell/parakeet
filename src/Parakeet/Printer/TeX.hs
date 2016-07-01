{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Parakeet.Printer.TeX (
  tex
) where

import           Control.Monad.Parakeet (Parakeet, env)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Text.Printf (printf)
import           Text.QuasiEmbedFile (efile)
import qualified Text.TemplateParser as TP

import           Parakeet.Types.Token2
import qualified Parakeet.Types.Document as D
import qualified Parakeet.Types.Options as O

#if !MIN_VERSION_text(1,2,2)
instance PrintfArg Text where
  formatArg txt = formatString $ T.unpack txt
#endif

build :: Bool -> Int -> String -> Text
build verbose f
  | verbose   = T.pack . printf "\\%s{\\verb|%s|}" (fonts !! f)
  | otherwise = T.pack . printf "\\%s{%s}" (fonts !! f)
  where fonts = [ "Huge", "huge"
                , "LARGE", "Large", "large"
                , "normalsize"
                , "small" , "footnotesize", "scriptsize", "tiny" 
                ] :: [String]

texifyTitle :: String -> Text
texifyTitle title = T.pack $ printf "\\title{%s}" title

texifyAuthor :: String -> Text
texifyAuthor author = T.pack $ printf "\\author{%s}" author

texify :: Int -> [Token2] -> Text
texify offset tokens = T.concat $ map texify1 tokens
  where
    mainFont = clampFont $ 4 + offset
    kanjiFont = clampFont $ 6 + offset
    romajiFont = clampFont $ 5 + offset
    clampFont f | f < 0 = 0
                | f > 9 = 9
                | otherwise = f
    texify1 :: Token2 -> Text
    texify1 d = case d of
      Line         -> " \\\\ \n"
      Lit s        -> build True mainFont s `T.append` " "
      Kanji k h r  -> T.pack $ printf "\\ruby{%s%s}{%s} " (build False mainFont k) (build False kanjiFont ("(" ++ concat h ++ ")")) (build False romajiFont (unwords r))
      Hiragana h r -> T.pack $ printf "\\ruby{%s}{%s} " (build False mainFont h) (build False romajiFont (unwords r))
      Katakana k r -> T.pack $ printf "\\ruby{%s}{%s} " (build False mainFont k) (build False romajiFont (unwords r))

tex :: D.Document -> Parakeet Text
tex doc = do
  template <- env O.templateFile
  case template of
    Nothing -> return $ T.concat [efile|template.tex|]
    Just (_, template) -> TP.substitute template body title author
  where
    title = maybe T.empty (texifyTitle . D.title) (D.metaInfo doc)
    author = maybe T.empty (texifyAuthor . D.author) (D.metaInfo doc)
    body = T.concat [maybe T.empty (const "\\maketitle") (D.metaInfo doc), "\n\n", texify 0 (D.body doc)]
