{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Main where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import Data.JSString (pack, unpack)

import Parakeet

foreign import javascript unsafe "document.getElementById($1).value"
  getElement :: JSString -> IO JSString
foreign import javascript unsafe "document.getElementById($1).value = $2;"
  setElement :: JSString -> JSString -> IO ()
foreign import javascript unsafe "parakeet = $1"
  setParakeet :: Callback a -> IO ()

main :: IO ()
main = do
  callback <- syncCallback ContinueAsync $ do
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
  setParakeet callback
