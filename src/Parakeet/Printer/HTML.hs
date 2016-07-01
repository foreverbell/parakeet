{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Parakeet.Printer.HTML (
  html
) where

import           Control.Monad.Parakeet (Parakeet, env)
import           Data.Char (ord)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Text.Printf
import           Text.QuasiEmbedFile (efile)
import qualified Text.TemplateParser as TP

import           Parakeet.Types.Token2
import qualified Parakeet.Types.Document as D
import qualified Parakeet.Types.Options as O

#if !MIN_VERSION_text(1,2,2)
instance PrintfArg Text where
  formatArg txt = formatString $ T.unpack txt
#endif

encode :: String -> Text
encode xs = T.concat $ map f xs
  where
    f c = T.pack $ printf "&#x%x;" (ord c)

ruby :: Text -> Text -> Text
ruby b t = T.pack $ printf "<ruby>%s<rt>%s</rt></ruby>" b t

htmlify1 :: Token2 -> Text
htmlify1 d = case d of
  Line         -> " <br /> \n"
  Lit s        -> encode s
  Kanji k h r  -> ruby (T.pack $ printf "%s(%s)" (encode k) (encode (concat h))) (encode (unwords r))
  Hiragana h r -> ruby (encode h) (encode (unwords r))
  Katakana k r -> ruby (encode k) (encode (unwords r))

htmlify :: [Token2] -> Text
htmlify tokens = T.concat $ map htmlify1 tokens

html :: D.Document -> Parakeet Text
html doc = do
  template <- env O.templateFile
  case template of
    Nothing -> return $ T.concat [efile|template.html|]
    Just (_, template) -> TP.substitute template body title author
  where
    title = maybe T.empty (encode . D.title) (D.metaInfo doc)
    author = maybe T.empty (encode . D.author) (D.metaInfo doc)
    body = htmlify (D.body doc)
