module Render (
  renderFile
) where

import           Control.Monad (liftM)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)

import qualified TexDoc as TD
import           TexDoc ((<||>))
import           Parser.Parser (doParse)
import qualified UTF8IO as IO

construct :: String -> String -> TD.TexDoc
construct j r = TD.DocumentClass "article" 
           <||> TD.EmptyLine
           <||> TD.UsePackage "geometry" "a4paper"
           <||> TD.EmptyLine
           <||> TD.UsePackage "xeCJK" []
           <||> TD.UsePackage "ruby" []
           <||> TD.UsePackage "amsmath" []
           <||> TD.EmptyLine
           <||> TD.LineSpread "2.0"
           <||> TD.RubySep "-1.7ex"
           <||> TD.NoIndent
           <||> TD.EmptyLine
           <||> TD.CJKFont TD.Main "MS Mincho"
           <||> TD.CJKFont TD.Sans "MS Gothic"
           <||> TD.EmptyLine
           <||> TD.Document (doParse j r)

render :: String -> String -> Text
render j r = TD.texify $ construct j r

renderFile :: FilePath -> FilePath -> Maybe FilePath -> IO ()
renderFile jf rf output = do
  jpnese <- IO.readFile jf
  romaji <- IO.readFile rf
  let d = T.unpack $ render jpnese romaji
  case output of
    Nothing -> putStrLn d
    Just f  -> IO.writeFile f d
