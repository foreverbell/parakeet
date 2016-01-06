module Main where

import qualified System.IO.UTF8 as IO
import           Test.HUnit
import           Text.Parakeet

defaultOptions :: Options 
defaultOptions = Options {
  optContent    = ([], [])
, optJInputFile = []
, optRInputFile = []
, optOutput     = InBareTex
, optFurigana   = InHiragana
, optMincho     = "MS Mincho"
, optGothic     = "MS Gothic"
, optNoMeta     = False
, optKeepLV     = False
}

getOptions :: String -> Bool -> IO Options
getOptions test keepLV = do
  let jf = "test-suite/" ++ test ++ "/" ++ test ++ ".j"
  let rf = "test-suite/" ++ test ++ "/" ++ test ++ ".r"
  j <- IO.readFile jf
  r <- IO.readFile rf
  return defaultOptions { optJInputFile = jf
                        , optRInputFile = rf 
                        , optContent = (j, r)
                        , optKeepLV = keepLV
                        }

getTest :: String -> Bool -> IO Test
getTest testName keepLV = do
  opts <- getOptions testName keepLV
  let result = parakeet opts 
  output <- case result of
              Left err -> fail err
              Right r  -> return (lines r)
  expect <- lines <$> IO.readFile ("test-expect/" ++ testName ++ ".tex.expect")
  return $ TestLabel testName $ TestCase $ do
    let nlines = length expect
    let singleLineTest (l, e, o) = assertEqual ("On line " ++ show (l + 1)) e o
    assertEqual "The number of lines" nlines (length output)
    mapM_ singleLineTest $ zip3 [0 .. nlines - 1] expect output

main :: IO Counts
main = do
  test1 <- getTest "Anonymous" False
  test2 <- getTest "Butter-fly" False
  test3 <- getTest "Nagori-yuki" True
  runTestTT $ TestList [test1, test2, test3]
