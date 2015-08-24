module Main (main) where

import           System.Environment (getArgs, withArgs)

import           Options (Options(..), runOpts)
import           Print (prettyPrint)

main = do
  opts <- runOpts =<< getArgs
  let put = optOutput opts
  case prettyPrint opts of 
    Left err -> fail err
    Right r  -> put r

test = do
  withArgs (buildArgs "../tests/Anonymous/Anonymous.j" "../tests/Anonymous/Anonymous.r" "Anonymous.tex") main
  withArgs (buildArgs "../tests/Butter-fly/Butter-fly.j" "../tests/Butter-fly/Butter-fly.r" "Butter-fly.tex") main
  where buildArgs j r t = ["-j", j, "-r", r, "-o", t]

