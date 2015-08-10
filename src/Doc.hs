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
build fnt str = printf "\\%s{%s}" (fonts !! fnt) str
  where
    fonts = [ 
      "Huge", "huge", 
      "LARGE", "Large", "large", 
      "normalsize", "small", "footnotesize", "scriptsize", "tiny" ]

data ArticleDoc = Line
                | Break
                | JPChar JPChar
                deriving (Show)

data CJKFontType = Main | Sans deriving (Show)

data TexDoc = DocClass String
            | UsePackage String String
            | RubySep String
            | CJKFont CJKFontType String
            | Document [ArticleDoc]
            deriving (Show)

data Doc = List [Doc]
         | Tex TexDoc
         deriving (Show)

texify :: Doc -> ByteString
texify doc = case doc of 
  List ds -> B.concat $ map texify ds
  Tex td  -> texifyTex td
 
texifyArticle :: ArticleDoc -> ByteString
texifyArticle doc = case doc of
  Line     -> B.pack $ "\\\\ \n"
  Break    -> B.pack $ "\\, "
  JPChar c -> B.pack $ (texifyJPChar c) ++ " "

texifyTex :: TexDoc -> ByteString
texifyTex doc = case doc of
  DocClass cls   -> B.pack $ printf "\\documentclass{%s}\n" cls
  UsePackage p d -> B.pack $ if null d
    then printf "\\usepackage{%s}\n" p
    else printf "\\usepackage[%s]{%s}\n" d p
  RubySep sep    -> B.pack $ printf "\\renewcommand\\rubysep{%s}\n" sep
  CJKFont Main f -> B.pack $ printf "\\setCJKmainfont{%s}\n" f
  CJKFont Sans f -> B.pack $ printf "\\setCJKsansfont{%s}\n" f
  Document adoc  -> B.concat [ B.pack "\\begin{document}\n\n", B.concat (map texifyArticle adoc), B.pack "\n\\end{document}\n" ]

texifyJPChar :: JPChar -> String
texifyJPChar (Kanji h j r) = printf "\\ruby{%s(%s)}{%s}" (build 0 h) (build 2 j) (build 5 r)
texifyJPChar (Hiragana j r) = printf "\\ruby{%s}{%s}" (build 0 j) (build 5 r)
texifyJPChar (Katakana j r) = printf "\\ruby{%s}{%s}" (build 0 j) (build 5 r)

