module Render (
  renderFile
) where

import           Control.Monad (liftM)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)

import qualified TexDoc as D
import           TexDoc ((<||>))
import           Parser.Parser (doParse)
import qualified UTF8IO as IO

construct :: String -> String -> D.TexDoc
construct j r = D.DocumentClass "article" 
           <||> D.EmptyLine
           <||> D.UsePackage "geometry" "a4paper"
           <||> D.EmptyLine
           <||> D.UsePackage "xeCJK" []
           <||> D.UsePackage "ruby" []
           <||> D.UsePackage "amsmath" []
           <||> D.EmptyLine
           <||> D.LineSpread "2.0"
           <||> D.RubySep "-1.7ex"
           <||> D.NoIndent
           <||> D.EmptyLine
           <||> D.CJKFont D.Main "MS Mincho"
           <||> D.CJKFont D.Sans "MS Gothic"
           <||> D.EmptyLine
           <||> D.Document (doParse j r)

render :: String -> String -> Text
render j r = D.texify $ construct j r

renderFile :: FilePath -> FilePath -> Maybe FilePath -> IO ()
renderFile jf rf output = do
  jpnese <- IO.readFile jf
  romaji <- IO.readFile rf
  let d = T.unpack $ render jpnese romaji
  case output of
    Nothing -> putStrLn d
    Just f  -> IO.writeFile f d
