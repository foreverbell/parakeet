module Main (main) where

import System.Environment (getArgs, withArgs)
import Options (Options(..), runOpts)

import           Print (prettyPrint)
import qualified UTF8IO as IO

main = do
  opts <- runOpts =<< getArgs
  let Options { optJInputFile = jf
              , optRInputFile = rf
              , optOutput     = op } = opts
  j <- IO.readFile jf
  r <- IO.readFile rf
  op $ prettyPrint opts j r

test = do
  withArgs (buildArgs "../tests/Anonymous/Anonymous.j" "../tests/Anonymous/Anonymous.r" "Anonymous.tex") main
  withArgs (buildArgs "../tests/Butter-fly/Butter-fly.j" "../tests/Butter-fly/Butter-fly.r" "Butter-fly.tex") main
  where buildArgs j r t = ["--no-wrap", "-j", j, "-r", r, "-o", t]

