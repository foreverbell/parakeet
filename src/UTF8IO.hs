module UTF8IO (
  readFile
, writeFile
) where

import           Control.Exception (bracket)
import           System.IO (openFile, hClose, hSetEncoding, IOMode(..), utf8)
import qualified Data.Text.IO as T
import           Data.Text (pack, unpack)
import           Prelude hiding (readFile, writeFile)

readFile :: FilePath -> IO String
readFile path = unpack <$> bracket open hClose T.hGetContents
  where
    open = do
      h <- openFile path ReadMode
      hSetEncoding h utf8
      return h

writeFile :: FilePath -> String -> IO ()
writeFile path t = bracket open hClose $ \h -> T.hPutStr h $ pack t
  where 
    open = do
      h <- openFile path WriteMode
      hSetEncoding h utf8
      return h
