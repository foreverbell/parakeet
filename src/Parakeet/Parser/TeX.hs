{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Parakeet.Parser.TeX (
  tex
) where

import           Control.Monad.Parakeet (Parakeet, TemplateError (..), toException, throw, env)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Text.Parsec
import           Text.Printf (printf)
import           Text.QuasiEmbedFile (efile)
import qualified Text.TemplateParser as TP

import           Parakeet.Parser.Parser (ParsedDocument)
import           Parakeet.Types.FToken
import           Parakeet.Types.Meta 
import           Parakeet.Types.Options

build :: Bool -> Int -> String -> Text
build useVerb f
  | useVerb   = T.pack . printf "\\%s{\\verb|%s|}" (fonts !! f)
  | otherwise = T.pack . printf "\\%s{%s}" (fonts !! f)
  where fonts = [ "Huge", "huge"
                , "LARGE", "Large", "large"
                , "normalsize"
                , "small" , "footnotesize", "scriptsize", "tiny" 
                ] :: [String]

substituteTemplate :: String -> Text -> Text -> Parakeet Text
substituteTemplate template body meta = do
  chunks <- case runParser TP.templateParser () [] template of
                 Right chunks -> return chunks
                 Left err -> throw $ toException (TemplateError $ printf "invalid template: %s." (show err))
  T.concat <$> mapM substitute chunks
  where
    substitute (TP.Lit l) = return $ T.pack l
    substitute (TP.Value v) = case v of
      "body" -> return body
      "meta" -> return meta
      _      -> throw $ toException (TemplateError $ printf "invalid placeholder %s." v)

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
      Lit s        -> build useVerb mainFont s `T.append` " "
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

tex :: ParsedDocument -> Parakeet Text
tex (meta0, tokens) = do
  let title = maybe T.empty (texifyTitle . getTitle) meta0
  -- TODO: using lit author is workaround, since ruby is not well supported in \author{ }
  let author = maybe T.empty (texifyAuthor . getLitAuthor) meta0
  let date = maybe T.empty (const "\\date{ }") meta0
  let meta = T.concat [title, "\n", author, "\n", date]
  let body = T.concat [maybe T.empty (const "\\maketitle") meta0, "\n\n", texify True 0 tokens]
  template <- env optTemplate
  case template of
       Nothing            -> return $ T.concat [efile|template.tex|]
       Just (_, template) -> substituteTemplate template body meta
