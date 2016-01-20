{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Parakeet.Translator.Tex (
  texBare
, tex
) where

import           Control.Monad.Parakeet (Parakeet, env)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Text.Printf (printf)
import           Text.QuasiEmbedFile (efile)
import           Prelude hiding (print)

import           Parakeet.Types.FToken
import           Parakeet.Types.Options
import           Parakeet.Types.Meta 

build :: Bool -> Int -> String -> String
build useVerb f
  | useVerb   = printf "\\%s{\\verb|%s|}" (fonts !! f)
  | otherwise = printf "\\%s{%s}" (fonts !! f)
  where fonts = [ "Huge", "huge"
                , "LARGE", "Large", "large"
                , "normalsize"
                , "small" , "footnotesize", "scriptsize", "tiny" 
                ] :: [String]

texify :: Bool -> Int -> [FToken] -> Text
texify useVerb offset tokens = T.concat $ map singleTexify tokens
  where
    mainFont = fixFont $ 4 + offset
    rubyFont = fixFont $ 6 + offset
    romajiFont = fixFont $ 5 + offset
    fixFont f | f < 0 = 0
              | f > 9 = 9
              | otherwise = f
    singleTexify :: FToken -> Text
    singleTexify d = case d of
      Line         -> " \\\\ \n"
      Lit s        -> T.pack $ build useVerb mainFont s ++ " "
      Kanji k h r  -> T.pack $ printf "\\ruby{%s%s}{%s} " (build False mainFont k) (build False rubyFont ("(" ++ concat h ++ ")")) (build False romajiFont (unwords r))
      Hiragana h r -> T.pack $ printf "\\ruby{%s}{%s} " (build False mainFont h) (build False romajiFont (unwords r))
      Katakana k r -> T.pack $ printf "\\ruby{%s}{%s} " (build False mainFont k) (build False romajiFont (unwords r))

texifyTitle :: [FToken] -> Text
texifyTitle title = T.pack $ printf "\\title{%s}" (T.unpack tex)
  where
    tex = texify False (-2) title

texifyAuthor :: [FToken] -> Text
texifyAuthor author = T.pack $ printf "\\author{%s}" (T.unpack tex)
  where
    tex = texify False 1 author

texBare :: (Maybe Meta, [FToken]) -> Parakeet Text
texBare (meta, tokens) = return $ T.concat [title, "\n", author, "\n\n", body]
  where
    title  = maybe T.empty (texifyTitle . getTitle) meta
    author = maybe T.empty (texifyAuthor . getLitAuthor) meta
    body   = texify True 0 tokens

tex :: (Maybe Meta, [FToken]) -> Parakeet Text
tex (meta0, tokens) = do
  mincho <- env optMincho
  gothic <- env optGothic
  let title = maybe T.empty (texifyTitle . getTitle) meta0
  -- TODO: using lit author is workaround, since ruby is not well supported in \author{ }
  let author = maybe T.empty (texifyAuthor . getLitAuthor) meta0
  let date = maybe T.empty (const "\\date{ }") meta0
  let meta = T.concat [title, "\n", author, "\n", date]
  let font = T.concat [T.pack $ printf "\\setCJKmainfont{%s}" mincho, "\n", T.pack $ printf "\\setCJKsansfont{%s}" gothic]
  let body = maybe T.empty (const "\\maketitle\n\n") meta0 `T.append` texify True 0 tokens
  return $ T.concat [efile|template.tex|]
