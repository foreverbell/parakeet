{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Exception (throw)
import           Control.Monad (when)
import           Data.Char (toLower)
import           System.Console.GetOpt (getOpt, usageInfo, ArgOrder (..), OptDescr (..), ArgDescr (..))
import           System.Environment (getArgs)
import qualified System.IO.UTF8 as IO
import           System.Exit (exitFailure, exitSuccess)
import           Text.Parakeet

type OutputIO = String -> IO ()

data ExtraOptions = ExtraOptions {
  eoptOutputIO     :: OutputIO
, eoptShowHelp     :: Bool
, eoptDumpTemplate :: Bool
}

firstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (a, c) = f a >>= \b -> return (b, c)

defaultOptions :: Options 
defaultOptions = Options {
  optJInputFile = ([], [])
, optRInputFile = ([], [])
, optTemplate   = Nothing
, optOutput     = InTex
, optFurigana   = InHiragana
, optNoMeta     = False
, optKeepLV     = False
}

defaultExtraOptions :: ExtraOptions
defaultExtraOptions = ExtraOptions {
  eoptOutputIO     = putStr
, eoptShowHelp     = False
, eoptDumpTemplate = False
}

isEmptyFile :: File -> Bool
isEmptyFile (f, _) = null f

initFile :: FilePath -> IO File
initFile f = IO.readFile f >>= \c -> return (f, c)

bindJInputFile a = firstM $ \o -> do
  f <- initFile a
  return o { optJInputFile = f }

bindRInputFile a = firstM $ \o -> do
  f <- initFile a
  return o { optRInputFile = f }

bindTemplate a = firstM $ \o -> do
  f <- initFile a
  return o { optTemplate = Just f }

bindOutputIO a (o, eo) = return (o, eo { eoptOutputIO = IO.writeFile a })

bindFormat a = firstM $ \o -> do
  f <- format
  return $ o { optOutput = f } 
  where format = case map toLower a of
          "tex"          -> return InTex
          "plaintex"     -> return InPlainTex
          _              -> die "Bad output format."

bindFurigana a = firstM $ \o -> do
  f <- format
  return $ o { optFurigana = f }
  where format = case map toLower a of
          "hiragana" -> return InHiragana
          "katakana" -> return InKatakana
          _          -> die "Bad furigana format."

setNoMeta = firstM $ \o -> return o { optNoMeta = True }

setKeepLV = firstM $ \o -> return o { optKeepLV = True }

setDumpTemplate (o, eo) = return (o, eo { eoptDumpTemplate = True })

setShowHelp (o, eo) = return (o, eo { eoptShowHelp = True })

options :: [OptDescr ((Options, ExtraOptions) -> IO (Options, ExtraOptions))]
options = [ Option ['j'] ["japanese"]      (ReqArg bindJInputFile "FILE") "Japanese input file"
          , Option ['r'] ["romaji"]        (ReqArg bindRInputFile "FILE") "Romaji input file"
          , Option ['t'] ["template"]      (ReqArg bindTemplate   "FILE") "Template file"
          , Option ['o'] ["output"]        (ReqArg bindOutputIO   "FILE") "Output file"
          , Option [   ] ["dump-template"] (NoArg  setDumpTemplate      ) "Dump tex template"
          , Option [   ] ["format"]        (ReqArg bindFormat   "FORMAT") "Output format: tex, plaintex" 
          , Option [   ] ["furigana"]      (ReqArg bindFurigana "FORMAT") "Furigana format: hiragana, katakana"
          , Option [   ] ["no-meta"]       (NoArg  setNoMeta            ) "Ignore meta data (title & author)"
          , Option [   ] ["keep-lv"]       (NoArg  setKeepLV            ) "Keep long vowel macron in output"
          , Option ['h'] ["help"]          (NoArg  setShowHelp          ) "Show help"
          ]

die :: String -> IO a
die e = do
  putStrLn $ e ++ "\n" ++ usageInfo "Usage: " options
  exitFailure

checkFile :: Options -> IO Options
checkFile opts = do
  let (jName, rName) = (fst $ optJInputFile opts, fst $ optRInputFile opts)
  when (null jName || null rName) $ die "Missing inputs."
  return opts

runOpts :: [String] -> IO (Options, ExtraOptions)
runOpts argv = case getOpt Permute options argv of
  (a, _, [])  -> foldl (>>=) (return (defaultOptions, defaultExtraOptions)) a
  (_, _, err) -> die $ concat err

main :: IO ()
main = do
  (opts, ExtraOptions {..}) <- runOpts =<< getArgs
  when eoptShowHelp $ putStrLn (usageInfo "Usage: " options) >> exitSuccess
  when eoptDumpTemplate $ putStr template >> exitSuccess
  checkFile opts
  case parakeet opts of 
       Left err -> throw err
       Right r  -> eoptOutputIO r
