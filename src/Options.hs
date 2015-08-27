module Options (
  Options(..)
, FuriganaFormat(..)
, runOpts
) where

import           System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import           Control.Monad (when)

import qualified UTF8IO as IO

data FuriganaFormat = InDefault | InHiragana | InKatakana deriving (Eq)

data Options = Options {
  optContent    :: (String, String)
, optJInputFile :: FilePath
, optRInputFile :: FilePath
, optOutput     :: String -> IO ()
, optNoWrap     :: Bool 
, optShowBreak  :: Bool
, optFurigana   :: FuriganaFormat
}

initOptions :: Options 
initOptions = Options {
  optContent    = ([], [])
, optJInputFile = []
, optRInputFile = []
, optOutput     = putStr
, optNoWrap     = False
, optShowBreak  = False
, optFurigana   = InDefault
}

bindJInputFile a o = return o { optJInputFile = a }
bindRInputFile a o = return o { optRInputFile = a }
bindOutput     a o = return o { optOutput     = IO.writeFile a }

setNoWrap    o = return o { optNoWrap    = True }
setShowBreak o = return o { optShowBreak = True }

furiganaError = "furigana option conflict"

setHiragana o = do
  when (f == InKatakana) $ die furiganaError
  return o { optFurigana = InHiragana }
  where f = optFurigana o

setKatakana o = do
  when (f == InHiragana) $ die furiganaError
  return o { optFurigana = InKatakana }
  where f = optFurigana o

options :: [OptDescr (Options -> IO Options)]
options = 
  [ Option ['j'] ["japanese"]   (ReqArg bindJInputFile "FILE") "japanese input file"
  , Option ['r'] ["romaji"]     (ReqArg bindRInputFile "FILE") "romaji input file"
  , Option ['o'] ["output"]     (ReqArg bindOutput     "FILE") "output file (default stdout)"
  , Option ['w'] ["no-wrap"]    (NoArg  setNoWrap            ) "disable wrapped tex output" 
  , Option ['b'] ["show-break"] (NoArg  setShowBreak         ) "show break from romaji file"
  , Option ['H'] ["hiragana"]   (NoArg  setHiragana          ) "set furigana format to hiragana (default)"
  , Option ['K'] ["katakana"]   (NoArg  setKatakana          ) "set furigana format to katakana"
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
      when (null jf || null rf) $ do
        die "Missing inputs\n"
      j <- IO.readFile jf
      r <- IO.readFile rf
      return opts { optContent = (j, r) }
      where jf = optJInputFile opts
            rf = optRInputFile opts
