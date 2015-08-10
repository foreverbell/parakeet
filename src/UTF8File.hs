module UTF8File (
  readFile
, writeFile
) where

import qualified Data.Text.IO as IO
import qualified Data.Text as T
import           Data.Text (Text)
import           System.IO (openFile, hClose, hSetEncoding, IOMode(..), utf8)
import           Control.Exception (bracket)
import           Prelude hiding (readFile, writeFile)

readFile :: FilePath -> IO Text
readFile path = bracket open hClose IO.hGetContents
  where
    open = do
      h <- openFile path ReadMode
      hSetEncoding h utf8
      return h

writeFile :: FilePath -> Text -> IO ()
writeFile path t = bracket open hClose $ \h -> IO.hPutStr h t
  where 
    open = do
      h <- openFile path WriteMode
      hSetEncoding h utf8
      return h

