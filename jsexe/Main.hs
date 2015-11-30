{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Main where

import GHCJS.Foreign
import GHCJS.Types
import Data.JSString (pack, unpack)

import Parakeet

foreign import javascript unsafe "document.getElementById($1).value"
  getElement :: JSString -> IO JSString
foreign import javascript unsafe "document.getElementById($1).value = $2;"
  setElement :: JSString -> JSString -> IO ()

main :: IO ()
main = do
  putStrLn "parakeet client version, powered by ghcjs."
  jap <- getElement (pack "jap")
  rom <- getElement (pack "rom")
  let options = Options {
    optContent    = (unpack jap, unpack rom)
  , optJInputFile = "japanese"
  , optRInputFile = "romaji"
  , optOutputIO   = undefined
  , optOutput     = InTex
  , optFurigana   = InHiragana
  , optMincho     = "MS Mincho"
  , optGothic     = "MS Gothic"
  , optShowBreak  = False
  , optNoMetaInfo = False
  , optKeepLV     = True
  }
  let result = case runParakeet options parakeet of
                      Left a -> unlines ["Error:", a]
                      Right b -> b
  setElement (pack "result") (pack result)
