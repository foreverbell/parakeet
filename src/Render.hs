module Render (

) where

import           Control.Monad (liftM)
import qualified Data.Text as T
import           Data.Text (Text)
import           JPChar
import qualified UTF8File as U8F

render :: String -> String -> String
render = undefined

renderFile :: FilePath -> FilePath -> FilePath -> IO ()
renderFile jf rf output = do
  japanese  <- T.unpack `liftM` U8F.readFile jf
  romanized <- T.unpack `liftM` U8F.readFile rf
  U8F.writeFile output . T.pack $ render japanese romanized

