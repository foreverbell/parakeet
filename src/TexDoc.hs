module TexDoc (
  CJKFontType(..)
, TexDoc(..)
, texify
, (<||>)
) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Text.Printf (printf)

import qualified TexElem as E

data CJKFontType = Main | Sans deriving (Show)

data TexDoc = Concat TexDoc TexDoc
            | EmptyLine
            | DocumentClass String
            | UsePackage String String
            | LineSpread String
            | RubySep String
            | NoIndent
            | CJKFont CJKFontType String
            | Document [E.TexElem]
            deriving (Show)

texify :: TexDoc -> Text
texify doc = case doc of
  Concat d1 d2    -> T.append (texify d1) (texify d2)
  EmptyLine       -> T.pack $ "\n"
  DocumentClass c -> T.pack $ printf "\\documentclass{%s}\n" c
  UsePackage p d  -> T.pack $ if null d
    then printf "\\usepackage{%s}\n" p
    else printf "\\usepackage[%s]{%s}\n" d p
  LineSpread spr  -> T.pack $ printf "\\linespread{%s}\n" spr
  RubySep sep     -> T.pack $ printf "\\renewcommand\\rubysep{%s}\n" sep
  NoIndent        -> T.pack $ "\\setlength\\parindent{0.0pt}\n"
  CJKFont Main f  -> T.pack $ printf "\\setCJKmainfont{%s}\n" f
  CJKFont Sans f  -> T.pack $ printf "\\setCJKsansfont{%s}\n" f
  Document es     -> T.concat [ T.pack "\\begin{document}\n\n", T.concat (map E.texify es), T.pack "\n\n\\end{document}\n" ]

(<||>) :: TexDoc -> TexDoc -> TexDoc
d1 <||> d2 = Concat d1 d2
