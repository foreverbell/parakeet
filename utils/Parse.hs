import           Text.Regex.PCRE
import           Text.Regex.Base.RegexLike
import qualified Data.Text.IO as IO
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Codec.Binary.UTF8.String (encodeString, decodeString)
import           System.IO (openFile, hClose, hSetEncoding, IOMode(..), utf8)
import           Control.Exception (bracket)

readUTF8 :: FilePath -> IO Text
readUTF8 path = bracket open hClose IO.hGetContents
  where
    open = do
      h <- openFile path ReadMode
      hSetEncoding h utf8
      return h

writeUTF8 :: FilePath -> Text -> IO ()
writeUTF8 path t = bracket open hClose $ \h -> IO.hPutStr h t
  where 
    open = do
      h <- openFile path WriteMode
      hSetEncoding h utf8
      return h

parse :: String -> [(String, String)]
parse t = helper (B.pack t) [] 
  where
    helper t xs = case parse' t of
      (_, _, _, [])      -> reverse xs
      (_, _, t', [a, b]) -> helper t' ((B.unpack a, B.unpack b) : xs)
    
    parse' :: ByteString -> (ByteString, ByteString, ByteString, [ByteString])
    parse' t = regex `match` t
      where
        regex = makeRegexOpts (defaultCompOpt + compMultiline) defaultExecOpt
                  "<td align=\"center\" bgcolor=\"white\"><span class=\"main\"><big><big>(.*?).</big></big>.<br>([a-z]+)</span></td>"

flatten :: [(String, String)] -> String
flatten xs = "[\n" ++ concatMap f (zip [1 .. ] xs) ++ "]"
  where
    l = length xs
    f (i, (j, r)) = h ++ "(\"" ++ (decodeString j) ++ "\", \"" ++ r ++ "\")" ++ t
      where t | i == l         = " "
              | i `mod` 5 == 0 = ",\n"
              | otherwise      = ", "
            h | i `mod` 5 == 1 = "  "
              | otherwise      = ""

main = do
  writeUTF8 "hiragana.out" . work =<< readUTF8 "../res/hiragana.html.txt"
  writeUTF8 "katakana.out" . work =<< readUTF8 "../res/katakana.html.txt"
    where work = T.pack . flatten . parse . encodeString . T.unpack

