module Main (main) where

import System.Environment (getArgs)
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.IO.Error (ioError, userError)
import Data.List (find)
import Data.Maybe (isJust, fromJust)
import Render (renderFile)

data OptFlag = JInput String
             | RInput String
             | Output String
             deriving (Show)

optType (JInput _) = 1
optType (RInput _) = 2
optType (Output _) = 3

options :: [OptDescr OptFlag]
options = 
  [ Option ['j'] ["japanese"] (ReqArg JInput "FILE") "japanese input file"
  , Option ['r'] ["romaji"]   (ReqArg RInput "FILE") "romaji input file"
  , Option ['o'] ["output"]   (ReqArg Output "FILE") "output file (default stdout)"
  ]

runOpts :: [String] -> IO (OptFlag, OptFlag, Maybe OptFlag)
runOpts argv = case getOpt Permute options argv of
  (o, n, [])  -> let j = find ((==) 1 . optType) o
                     r = find ((==) 2 . optType) o
                     t = find ((==) 3 . optType) o
                 in  if isJust j && isJust r
                       then return (fromJust j, fromJust r, t)
                       else die "Missing inputs\n"
  (_, _, err) -> die $ concat err
  where header = "Usage: "
        die e  = ioError (userError (e ++ usageInfo header options))

main = do
  (JInput j, RInput r, o) <- runOpts =<< getArgs
  renderFile j r $ case o of
    Just (Output f) -> Just f
    Nothing         -> Nothing

test = do
  renderFile "../tests/Butter-fly/Butter-fly.j" "../tests/Butter-fly/Butter-fly.r" $ Just "Butter-fly.tex"
  renderFile "../tests/Anonymous/Anonymous.j" "../tests/Anonymous/Anonymous.r" $ Just "Anonymous.tex"
