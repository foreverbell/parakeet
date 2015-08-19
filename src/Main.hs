module Main (main) where

import           System.Environment (getArgs)
import           System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import           System.IO.Error (ioError, userError)
import           Control.Monad (when, void)

import           Print (prettyPrint)
import qualified UTF8IO as IO

data Options = Options {
  optJInputFile :: FilePath,
  optRInputFile :: FilePath,
  optOutput     :: String -> IO ()
}

initOptions :: Options 
initOptions = Options {
  optJInputFile = [],
  optRInputFile = [],
  optOutput     = putStr
}

bindJInputFile a o = return o { optJInputFile = a }
bindRInputFile a o = return o { optRInputFile = a }
bindOutput     a o = return o { optOutput     = IO.writeFile a }

options :: [OptDescr (Options -> IO Options)]
options = 
  [ Option ['j'] ["japanese"] (ReqArg bindJInputFile "FILE") "japanese input file"
  , Option ['r'] ["romaji"]   (ReqArg bindRInputFile "FILE") "romaji input file"
  , Option ['o'] ["output"]   (ReqArg bindOutput     "FILE") "output file (default stdout)"
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

main = do
  opts <- runOpts =<< getArgs
  let Options { optJInputFile = jf
              , optRInputFile = rf
              , optOutput     = op } = opts
  j <- IO.readFile jf
  r <- IO.readFile rf
  op $ prettyPrint j r

testPrettyPrint jf rf output = do
  j <- IO.readFile jf
  r <- IO.readFile rf
  IO.writeFile output $ prettyPrint j r

test = do
  testPrettyPrint "../tests/Anonymous/Anonymous.j" "../tests/Anonymous/Anonymous.r" "Anonymous.tex"
  testPrettyPrint "../tests/Butter-fly/Butter-fly.j" "../tests/Butter-fly/Butter-fly.r" "Butter-fly.tex"
