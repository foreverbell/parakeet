{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad (when)
import           Data.Char (toLower)
import           Data.Default.Class (Default (..))
import           Data.Maybe (fromJust)
import           System.Console.GetOpt (getOpt, usageInfo, ArgOrder (..), OptDescr (..), ArgDescr (..))
import           System.Directory (getTemporaryDirectory, getCurrentDirectory, setCurrentDirectory, makeAbsolute, copyFile)
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import qualified System.IO.UTF8 as IO
import           System.IO (hClose)
import           System.IO.Temp (openTempFile)
import           System.Process (callProcess)
import           Text.Parakeet

data ExtraOptions = ExtraOptions {
  outputPath       :: Maybe FilePath
, showHelp         :: Bool
, showTeXTemplate  :: Bool
, showHTMLTemplate :: Bool
}

firstM :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (a, c) = f a >>= \b -> return (b, c)

instance Default Options where
  def = Options { inputFileJ   = ([], [])
                , inputFileR   = ([], [])
                , templateFile = Nothing
                , furigana     = InHiragana
                , noMeta       = False
                , keepLV       = False
                }

instance Default ExtraOptions where
  def = ExtraOptions { outputPath       = Nothing
                     , showHelp         = False
                     , showTeXTemplate  = False
                     , showHTMLTemplate = False
                     }

isEmptyFile :: File -> Bool
isEmptyFile (f, _) = null f

initFile :: FilePath -> IO File
initFile f = IO.readFile f >>= \c -> return (f, c)

bindInputFileJ a = firstM $ \o -> do
  f <- initFile a
  return o { inputFileJ = f }

bindInputFileR a = firstM $ \o -> do
  f <- initFile a
  return o { inputFileR = f }

bindTemplateFile a = firstM $ \o -> do
  f <- initFile a
  return o { templateFile = Just f }

bindOutputPath a (o, eo) = return (o, eo { outputPath = Just a })

bindFurigana a = firstM $ \o -> do
  f <- format
  return $ o { furigana = f }
  where format = case map toLower a of
          "hiragana" -> return InHiragana
          "katakana" -> return InKatakana
          _          -> die "Bad furigana format."

setNoMeta = firstM $ \o -> return o { noMeta = True }

setKeepLV = firstM $ \o -> return o { keepLV = True }

setShowTeXTemplate (o, eo) = return (o, eo { showTeXTemplate = True })

setShowHTMLTemplate (o, eo) = return (o, eo { showHTMLTemplate = True })

setShowHelp (o, eo) = return (o, eo { showHelp = True })

options :: [OptDescr ((Options, ExtraOptions) -> IO (Options, ExtraOptions))]
options = [ Option ['j'] ["japanese"]      (ReqArg bindInputFileJ   "FILE") "Japanese input file"
          , Option ['r'] ["romaji"]        (ReqArg bindInputFileR   "FILE") "Romaji input file"
          , Option ['t'] ["template"]      (ReqArg bindTemplateFile "FILE") "Template file"
          , Option ['o'] ["output"]        (ReqArg bindOutputPath   "FILE") "Output file path"
          , Option [   ] ["template-tex"]  (NoArg  setShowTeXTemplate     ) "Show TeX template"
          , Option [   ] ["template-html"] (NoArg  setShowHTMLTemplate    ) "Show HTML template"
          , Option [   ] ["furigana"]      (ReqArg bindFurigana   "FORMAT") "Furigana format: hiragana, katakana"
          , Option [   ] ["no-meta"]       (NoArg  setNoMeta              ) "Ignore title and author metadata"
          , Option [   ] ["keep-lv"]       (NoArg  setKeepLV              ) "Keep long vowel macron in output"
          , Option ['h'] ["help"]          (NoArg  setShowHelp            ) "Show help"
          ]

die :: String -> IO a
die e = do
  putStrLn $ e ++ "\n" ++ usageInfo "Usage: " options
  exitFailure

checkFile :: Options -> IO Options
checkFile opts = do
  let (jName, rName) = (fst $ inputFileJ opts, fst $ inputFileR opts)
  when (null jName || null rName) $ die "Missing inputs."
  return opts

runOpts :: [String] -> IO (Options, ExtraOptions)
runOpts argv = case getOpt Permute options argv of
  (a, _, [])  -> foldl (>>=) (return (def, def)) a
  (_, _, err) -> die $ concat err

xelatex :: FilePath -> String -> IO ()
xelatex filePath buffer = do
  tmpDir <- getTemporaryDirectory
  curDir <- getCurrentDirectory
  (tmp, h) <- openTempFile tmpDir "parakeet.tex"
  hClose h
  setCurrentDirectory tmpDir
  IO.writeFile tmp buffer
  callProcess "xelatex" [tmp]
  setCurrentDirectory curDir
  let sourcePath = take (length tmp - 3) tmp ++ "pdf"
  targetPath <- makeAbsolute filePath
  putStrLn $ "Copying " ++ sourcePath ++ " to " ++ targetPath ++ "."
  copyFile sourcePath targetPath

main :: IO ()
main = do
  (opts, ExtraOptions {..}) <- runOpts =<< getArgs
  when showHelp $ putStrLn (usageInfo "Usage: " options) >> exitSuccess
  when showTeXTemplate $ putStr templateTeX >> exitSuccess
  when showHTMLTemplate $ putStr templateHTML >> exitSuccess
  checkFile opts
  let ext = map toLower . extName <$> outputPath
  let (format, io) = case ext of
                       Nothing -> (TeXFormat, putStr)
                       Just "pdf" -> (TeXFormat, xelatex (fromJust outputPath))
                       Just "htm" -> (HTMLFormat, IO.writeFile (fromJust outputPath))
                       Just "html" -> (HTMLFormat, IO.writeFile (fromJust outputPath))
                       _ -> (TeXFormat, IO.writeFile (fromJust outputPath))
  case parakeet opts format of 
    Left err -> putStrLn (show err)
    Right r  -> io r
  where
    extName = reverse . takeWhile ((/=) '.') . reverse
