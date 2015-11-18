module Parakeet.Options (
  Options(..)
, OutputFormat(..)
, FuriganaFormat(..)
, runOpts
) where

import           System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import qualified System.IO.UTF8 as IO
import           Control.Monad (when)
import           Data.Char.Extra (toLower)

data OutputFormat = InTex | InBareTex | InIntermediate deriving (Eq)
data FuriganaFormat = InHiragana | InKatakana deriving (Eq)

data Options = Options {
  optContent    :: (String, String)
, optJInputFile :: FilePath
, optRInputFile :: FilePath
, optOutputIO   :: String -> IO ()
, optOutput     :: OutputFormat
, optFurigana   :: FuriganaFormat
, optMincho     :: String
, optGothic     :: String
, optShowBreak  :: Bool
, optNoMetaInfo :: Bool
, optKeepLV     :: Bool
}

initOptions :: Options 
initOptions = Options {
  optContent    = ([], [])
, optJInputFile = []
, optRInputFile = []
, optOutputIO   = putStr
, optOutput     = InTex
, optFurigana   = InHiragana
, optMincho     = "MS Mincho"
, optGothic     = "MS Gothic"
, optShowBreak  = False
, optNoMetaInfo = False
, optKeepLV     = False
}

bindJInputFile a o = return o { optJInputFile = a }

bindRInputFile a o = return o { optRInputFile = a }

bindOutputIO a o = return o { optOutputIO = IO.writeFile a }

bindFormat a o = do
  f <- format
  return $ o { optOutput = f } 
  where format = case toLower a of
          "tex"          -> return InTex
          "baretex"      -> return InBareTex
          "intermediate" -> return InIntermediate
          _              -> die "Bad output format"

bindFurigana a o = do
  f <- format
  return $ o { optFurigana = f }
  where format = case toLower a of
          "hiragana" -> return InHiragana
          "katakana" -> return InKatakana
          _          -> die "Bad furigana format"

bindMincho a o = return $ o { optMincho = a }

bindGothic a o = return $ o { optGothic = a }

setShowBreak o = return o { optShowBreak = True }

setNoMetaInfo o = return o { optNoMetaInfo = True }

setKeepLV o = return o { optKeepLV = True }

options :: [OptDescr (Options -> IO Options)]
options = 
  [ Option ['j'] ["japanese"]   (ReqArg bindJInputFile "FILE") "japanese input file"
  , Option ['r'] ["romaji"]     (ReqArg bindRInputFile "FILE") "romaji input file"
  , Option ['o'] ["output"]     (ReqArg bindOutputIO   "FILE") "output file (default stdout)"
  , Option [   ] ["format"]     (ReqArg bindFormat   "FORMAT") "output format: tex (default) | baretex | intermediate" 
  , Option [   ] ["furigana"]   (ReqArg bindFurigana "FORMAT") "furigana format: hiragana (default) | katakana"
  , Option [   ] ["mincho"]     (ReqArg bindMincho     "FONT") "mincho font for tex output, \"MS Mincho\" by default"
  , Option [   ] ["gothic"]     (ReqArg bindGothic     "FONT") "gothic font for tex output, \"MS Gothic\" by default"
  , Option [   ] ["show-break"] (NoArg  setShowBreak         ) "show break from romaji file"
  , Option [   ] ["no-meta"]    (NoArg  setNoMetaInfo        ) "ignore metainfo (title & author)"
  , Option [   ] ["keep-lv"]    (NoArg  setKeepLV            ) "keep long vowel macron in output"
  ]

die :: String -> IO a
die e = fail $ e ++ usageInfo "Usage: " options

runOpts :: [String] -> IO Options
runOpts argv = case getOpt Permute options argv of
  (a, _, [])  -> do
     opts <- foldl (>>=) (return initOptions) a
     setFileContent opts  
  (_, _, err) -> die $ concat err
  where
    setFileContent opts = do
      when (null jf || null rf) $ die "Missing inputs\n"
      j <- IO.readFile jf
      r <- IO.readFile rf
      return opts { optContent = (j, r) }
      where jf = optJInputFile opts
            rf = optRInputFile opts
