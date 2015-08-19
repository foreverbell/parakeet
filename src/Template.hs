{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template (
  template
) where

import Text.RawString.QQ
import Data.Text.Lazy (Text)

template :: Text
template = [r|

\documentclass{article}

\usepackage[a4paper]{geometry}

\usepackage{xeCJK}
\usepackage{ruby}

\setCJKmainfont{MS Mincho}
\setCJKsansfont{MS Gothic}

\linespread{2.0}
\setlength\parindent{0.0pt}
\renewcommand\rubysep{-1.7ex}

\begin{document}

$body$

\end{document}

|]
