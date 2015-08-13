module IO (
  readFile
, writeFile
, putStrLn
) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Data.Text (Text)
import           System.IO (openFile, hClose, hSetEncoding, IOMode(..), utf8)
import           Control.Exception (bracket)
import           Prelude hiding (readFile, writeFile, putStrLn)

readFile :: FilePath -> IO Text
readFile path = bracket open hClose TIO.hGetContents
  where
    open = do
      h <- openFile path ReadMode
      hSetEncoding h utf8
      return h

writeFile :: FilePath -> Text -> IO ()
writeFile path t = bracket open hClose $ \h -> TIO.hPutStr h t
  where 
    open = do
      h <- openFile path WriteMode
      hSetEncoding h utf8
      return h

putStrLn :: Text -> IO ()
putStrLn = TIO.putStrLn
