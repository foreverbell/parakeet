module TexDoc (
  CJKFontType(..)
, TexDoc(..)
, texify
, (<||>)
) where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Text.Printf (printf)

import qualified ArticleDoc as AD

data CJKFontType = Main | Sans deriving (Show)

data TexDoc = Concat TexDoc TexDoc
            | EmptyLine
            | DocumentClass String
            | UsePackage String String
            | RubySep String
            | CJKFont CJKFontType String
            | Document AD.ArticleDoc
            deriving (Show)

texify :: TexDoc -> ByteString
texify doc = case doc of
  Concat d1 d2    -> B.append (texify d1) (texify d2)
  EmptyLine       -> B.pack $ "\n"
  DocumentClass c -> B.pack $ printf "\\documentclass{%s}\n" c
  UsePackage p d  -> B.pack $ if null d
    then printf "\\usepackage{%s}\n" p
    else printf "\\usepackage[%s]{%s}\n" d p
  RubySep sep     -> B.pack $ printf "\\renewcommand\\rubysep{%s}\n" sep
  CJKFont Main f  -> B.pack $ printf "\\setCJKmainfont{%s}\n" f
  CJKFont Sans f  -> B.pack $ printf "\\setCJKsansfont{%s}\n" f
  Document d      -> B.concat [ B.pack "\\begin{document}\n\n", AD.texify d, B.pack "\n\\end{document}\n" ]

(<||>) :: TexDoc -> TexDoc -> TexDoc
d1 <||> d2 = Concat d1 d2

