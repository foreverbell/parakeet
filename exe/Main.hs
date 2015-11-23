module Main (
  main
) where

import System.Environment (getArgs)

import Parakeet
import Options (runOpts)

main :: IO ()
main = do
  opts <- runOpts =<< getArgs
  let put = optOutputIO opts
  let res = runParakeet opts parakeet
  case res of 
    Left err -> fail err
    Right r  -> put r
