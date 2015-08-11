module Render (
  renderFile
) where

import           Control.Monad (liftM)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified TexDoc as TD
import           TexDoc ((<||>))
import           Parser (parseDoc)
import qualified IO as IO

construct :: String -> String -> TD.TexDoc
construct j r = TD.DocumentClass "article" 
           <||> TD.EmptyLine
           <||> TD.UsePackage "geometry" "a4paper"
           <||> TD.EmptyLine
           <||> TD.UsePackage "xeCJK" []
           <||> TD.UsePackage "ruby" []
           <||> TD.UsePackage "amsmath" []
           <||> TD.EmptyLine
           <||> TD.RubySep "-0.1ex"
           <||> TD.EmptyLine
           <||> TD.CJKFont TD.Main "MS Mincho"
           <||> TD.CJKFont TD.Sans "MS Gothic"
           <||> TD.EmptyLine
           <||> TD.Document (parseDoc j r)

render :: String -> String -> String
render = ((B.unpack . TD.texify) .) . construct

renderFile :: FilePath -> FilePath -> FilePath -> IO ()
renderFile jf rf output = do
  jpnese <- T.unpack `liftM` IO.readFile jf
  romaji <- T.unpack `liftM` IO.readFile rf
  IO.writeFile output . T.pack $ render jpnese romaji

