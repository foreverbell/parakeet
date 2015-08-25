module Options (
  Options(..)
, runOpts
) where

import           System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import           Control.Monad (when)

import qualified UTF8IO as IO

data Options = Options {
  optContent    :: (String, String)
, optJInputFile :: FilePath
, optRInputFile :: FilePath
, optOutput     :: String -> IO ()
, optNoWrap     :: Bool 
, optShowBreak  :: Bool
}

initOptions :: Options 
initOptions = Options {
  optContent    = ([], [])
, optJInputFile = []
, optRInputFile = []
, optOutput     = putStr
, optNoWrap     = False
, optShowBreak  = False
}

bindJInputFile a o = return o { optJInputFile = a }
bindRInputFile a o = return o { optRInputFile = a }
bindOutput     a o = return o { optOutput     = IO.writeFile a }

setNoWrap    o = return o { optNoWrap    = True }
setShowBreak o = return o { optShowBreak = True }

options :: [OptDescr (Options -> IO Options)]
options = 
  [ Option ['j'] ["japanese"]   (ReqArg bindJInputFile "FILE") "japanese input file"
  , Option ['r'] ["romaji"]     (ReqArg bindRInputFile "FILE") "romaji input file"
  , Option ['o'] ["output"]     (ReqArg bindOutput     "FILE") "output file (default stdout)"
  , Option ['w'] ["no-wrap"]    (NoArg  setNoWrap            ) "disable wrapped tex output" 
  , Option ['b'] ["show-break"] (NoArg  setShowBreak         ) "show break in romaji file"
  ]

die :: String -> IO a
die e = fail $ e ++ usageInfo "Usage: " options

setFileContent :: Options -> IO Options
setFileContent opts = do
  when (null jf || null rf) $ do
    die "Missing inputs\n"
  j <- IO.readFile jf
  r <- IO.readFile rf
  return opts { optContent = (j, r) }
  where jf = optJInputFile opts
        rf = optRInputFile opts

runOpts :: [String] -> IO Options
runOpts argv = case getOpt Permute options argv of
  (a, _, [])  -> do
     opts <- foldl (>>=) (return initOptions) a
     setFileContent opts  
  (_, _, err) -> die $ concat err

