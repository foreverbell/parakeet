module Doc (
  ArticleDoc(..)
, CJKFontType(..)
, TexDoc(..)
, Doc(..)
, texify
) where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Text.Printf (printf)
import           JPChar

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
                | JPChar JPChar
                deriving (Show)

data CJKFontType = Main | Sans deriving (Show)

data TexDoc = DocumentClass String
            | UsePackage String String
            | RubySep String
            | CJKFont CJKFontType String
            | Document ArticleDoc
            deriving (Show)

data Doc = DList [Doc]
         | Tex TexDoc
         deriving (Show)

texifyArticle :: ArticleDoc -> ByteString
texifyArticle doc = case doc of
  ADList d -> B.concat (map texifyArticle d)
  Line     -> B.pack $ "\\\\ \n"
  Break    -> B.pack $ "\\, "
  JPChar c -> B.pack $ (texifyJPChar c) ++ " "

texifyTex :: TexDoc -> ByteString
texifyTex doc = case doc of
  DocumentClass c -> B.pack $ printf "\\documentclass{%s}\n" c
  UsePackage p d  -> B.pack $ if null d
    then printf "\\usepackage{%s}\n" p
    else printf "\\usepackage[%s]{%s}\n" d p
  RubySep sep     -> B.pack $ printf "\\renewcommand\\rubysep{%s}\n" sep
  CJKFont Main f  -> B.pack $ printf "\\setCJKmainfont{%s}\n" f
  CJKFont Sans f  -> B.pack $ printf "\\setCJKsansfont{%s}\n" f
  Document d      -> B.concat [ B.pack "\\begin{document}\n\n", texifyArticle d, B.pack "\n\\end{document}\n" ]

texify :: Doc -> ByteString
texify doc = case doc of 
  DList ds -> B.concat $ map texify ds
  Tex td   -> texifyTex td
 
texifyJPChar :: JPChar -> String
texifyJPChar (Kanji h j r) = printf "\\ruby{%s(%s)}{%s}" (build 0 h) (build 2 j) (build 5 r)
texifyJPChar (Hiragana j r) = printf "\\ruby{%s}{%s}" (build 0 j) (build 5 r)
texifyJPChar (Katakana j r) = printf "\\ruby{%s}{%s}" (build 0 j) (build 5 r)
