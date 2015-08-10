module Render (
  renderFile
) where

import           Control.Monad (liftM)
import           Control.Applicative ((<$>))
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.ByteString.Lazy.Char8 (ByteString)
import           JPChar
import           Doc
import qualified UTF8File as U8F

parse :: String -> String -> [ArticleDoc]
parse j r = []

construct :: String -> String -> Doc
construct j r = List $ Tex <$> [
  DocClass "article",
  UsePackage "geometry" "a4paper",
  UsePackage "xeCJK" [],
  UsePackage "ruby" [],
  UsePackage "amsmath" [],
  RubySep "-0.1ex",
  CJKFont Main "MS Mincho",
  CJKFont Sans "MS Gothic",
  Document (parse j r) ]

render :: String -> String -> String
render = ((B.unpack . texify) .) . construct

renderFile :: FilePath -> FilePath -> FilePath -> IO ()
renderFile jf rf output = do
  japanese  <- T.unpack `liftM` U8F.readFile jf
  romanized <- T.unpack `liftM` U8F.readFile rf
  U8F.writeFile output . T.pack $ render japanese romanized

