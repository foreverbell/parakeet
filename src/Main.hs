module Main (main) where

import System.Environment (getArgs)

import Options (Options(..), runOpts)
import Print (prettyPrint)

main = do
  opts <- runOpts =<< getArgs
  let put = optOutput opts
  case prettyPrint opts of 
    Left err -> fail err
    Right r  -> put r
