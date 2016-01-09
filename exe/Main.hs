module Main where

import           Control.Exception (throw)
import           Control.Monad (when)
import           Data.Char (toLower)
import           System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import           System.Environment (getArgs)
import qualified System.IO.UTF8 as IO
import           Text.Parakeet

type OutputIO = String -> IO ()

firstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (a, c) = f a >>= \b -> return (b, c)

defaultOptions :: Options 
defaultOptions = Options {
  optContent    = ([], [])
, optJInputFile = []
, optRInputFile = []
, optOutput     = InTex
, optFurigana   = InHiragana
, optMincho     = "MS Mincho"
, optGothic     = "MS Gothic"
, optNoMeta     = False
, optKeepLV     = False
}

defaultOutput :: OutputIO
defaultOutput = putStr

bindJInputFile a = firstM $ \o -> return o { optJInputFile = a }

bindRInputFile a = firstM $ \o -> return o { optRInputFile = a }

bindOutputIO a (o, _) = return (o, IO.writeFile a)

bindFormat a = firstM $ \o -> do
  f <- format
  return $ o { optOutput = f } 
  where format = case map toLower a of
          "tex"          -> return InTex
          "baretex"      -> return InBareTex
          "intermediate" -> return InIntermediate
          _              -> die "Bad output format"

bindFurigana a = firstM $ \o -> do
  f <- format
  return $ o { optFurigana = f }
  where format = case map toLower a of
          "hiragana" -> return InHiragana
          "katakana" -> return InKatakana
          _          -> die "Bad furigana format"

bindMincho a = firstM $ \o -> return o { optMincho = a }

bindGothic a = firstM $ \o -> return o { optGothic = a }

setNoMeta = firstM $ \o -> return o { optNoMeta = True }

setKeepLV = firstM $ \o -> return o { optKeepLV = True }

options :: [OptDescr ((Options, OutputIO) -> IO (Options, OutputIO))]
options = 
  [ Option ['j'] ["japanese"]   (ReqArg bindJInputFile "FILE") "japanese input file"
  , Option ['r'] ["romaji"]     (ReqArg bindRInputFile "FILE") "romaji input file"
  , Option ['o'] ["output"]     (ReqArg bindOutputIO   "FILE") "output file (default stdout)"
  , Option [   ] ["format"]     (ReqArg bindFormat   "FORMAT") "output format: tex (default) | baretex | intermediate" 
  , Option [   ] ["furigana"]   (ReqArg bindFurigana "FORMAT") "furigana format: hiragana (default) | katakana"
  , Option [   ] ["mincho"]     (ReqArg bindMincho     "FONT") "mincho font for tex output, \"MS Mincho\" by default"
  , Option [   ] ["gothic"]     (ReqArg bindGothic     "FONT") "gothic font for tex output, \"MS Gothic\" by default"
  , Option [   ] ["no-meta"]    (NoArg  setNoMeta            ) "ignore meta data (title & author)"
  , Option [   ] ["keep-lv"]    (NoArg  setKeepLV            ) "keep long vowel macron in output"
  ]

die :: String -> IO a
die e = fail $ e ++ usageInfo "Usage: " options

runOpts :: [String] -> IO (Options, OutputIO)
runOpts argv = case getOpt Permute options argv of
  (a, _, [])  -> firstM readFileContent =<< foldl (>>=) (return (defaultOptions, defaultOutput)) a
  (_, _, err) -> die $ concat err
  where
    readFileContent opts = do
      let jf = optJInputFile opts
      let rf = optRInputFile opts
      when (null jf || null rf) $ die "Missing inputs\n"
      j <- IO.readFile jf
      r <- IO.readFile rf
      return opts { optContent = (j, r) }
       
main :: IO ()
main = do
  (opts, output) <- runOpts =<< getArgs
  let res = parakeet opts
  case res of 
    Left err -> throw err
    Right r  -> output r
