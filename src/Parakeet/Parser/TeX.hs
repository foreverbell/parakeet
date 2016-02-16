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

import           Parakeet.Types.FToken
import qualified Parakeet.Types.Document as D
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
      _      -> throw $ toException (TemplateError $ printf "invalid placeholder $%s$." v)

texify :: Bool -> Int -> [FToken] -> Text
texify useVerb offset tokens = T.concat $ map singleTexify tokens
  where
    mainFont = clampFont $ 4 + offset
    rubyFont = clampFont $ 6 + offset
    romajiFont = clampFont $ 5 + offset
    clampFont f | f < 0 = 0
                | f > 9 = 9
                | otherwise = f
    singleTexify :: FToken -> Text
    singleTexify d = case d of
      Line         -> " \\\\ \n"
      Lit s        -> build useVerb mainFont s `T.append` " "
      Kanji k h r  -> T.pack $ printf "\\ruby{%s%s}{%s} " (build False mainFont k) (build False rubyFont ("(" ++ concat h ++ ")")) (build False romajiFont (unwords r))
      Hiragana h r -> T.pack $ printf "\\ruby{%s}{%s} " (build False mainFont h) (build False romajiFont (unwords r))
      Katakana k r -> T.pack $ printf "\\ruby{%s}{%s} " (build False mainFont k) (build False romajiFont (unwords r))

texifyTitle :: String -> Text
texifyTitle title = T.pack $ printf "\\title{%s}" (T.unpack $ texify False (-2) [Lit title])

texifyAuthor :: String -> Text
texifyAuthor author = T.pack $ printf "\\author{%s}" (T.unpack $ texify False 1 [Lit author])

tex :: D.Document -> Parakeet Text
tex document = do
  let title  = maybe T.empty (texifyTitle . D.title) (D.meta document)
  let author = maybe T.empty (texifyAuthor . D.author) (D.meta document)
  let date   = maybe T.empty (const "\\date{ }") (D.meta document)
  let meta   = T.concat [title, "\n", author, "\n", date]
  let body   = T.concat [maybe T.empty (const "\\maketitle") (D.meta document), "\n\n", texify True 0 (D.body document)]
  template   <- env optTemplate
  case template of
       Nothing            -> return $ T.concat [efile|template.tex|]
       Just (_, template) -> substituteTemplate template body meta
