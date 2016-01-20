module System.IO.UTF8 (
  readFile
, writeFile
) where

import           Control.Exception (bracket)
import           System.IO (openFile, hClose, hSetEncoding, IOMode (..), Handle, utf8)
import qualified Data.Text.IO as T
import           Data.Text (pack, unpack)
import           Prelude hiding (readFile, writeFile)

open :: FilePath -> IOMode -> IO Handle
open path mode = do
  h <- openFile path mode
  hSetEncoding h utf8
  return h

readFile :: FilePath -> IO String
readFile path = unpack <$> bracket (open path ReadMode) hClose T.hGetContents

writeFile :: FilePath -> String -> IO ()
writeFile path t = bracket (open path WriteMode) hClose $ \h -> T.hPutStr h $ pack t
