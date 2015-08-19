module Options (
  Options(..)
, runOpts
) where

import           System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import           System.IO.Error (ioError, userError)
import           Control.Monad (when)

import qualified UTF8IO as IO

data Options = Options {
  optJInputFile :: FilePath,
  optRInputFile :: FilePath,
  optOutput     :: String -> IO (),
  optNoWrap     :: Bool
}

initOptions :: Options 
initOptions = Options {
  optJInputFile = [],
  optRInputFile = [],
  optOutput     = putStr,
  optNoWrap     = False
}

bindJInputFile a o = return o { optJInputFile = a }
bindRInputFile a o = return o { optRInputFile = a }
bindOutput     a o = return o { optOutput     = IO.writeFile a }

setNoWrap o = return o { optNoWrap = True }

options :: [OptDescr (Options -> IO Options)]
options = 
  [ Option ['j'] ["japanese"] (ReqArg bindJInputFile "FILE") "japanese input file"
  , Option ['r'] ["romaji"]   (ReqArg bindRInputFile "FILE") "romaji input file"
  , Option ['o'] ["output"]   (ReqArg bindOutput     "FILE") "output file (default stdout)"
  , Option ['w'] ["no-wrap"]  (NoArg  setNoWrap            ) "disable wrapped tex output" 
  ]

runOpts :: [String] -> IO Options
runOpts argv = case getOpt Permute options argv of
  (a, n, [])  -> do
     opts <- foldl (>>=) (return initOptions) a
     when (null (optJInputFile opts) || null (optRInputFile opts)) $ do
       die "Missing inputs\n"
     return opts  
  (_, _, err) -> die $ unlines err
  where die e  = ioError (userError (e ++ usageInfo "Usage: " options))
