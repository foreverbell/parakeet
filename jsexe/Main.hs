{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

-- * Warning: probably BROKEN.

module Main where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import Data.JSString (pack, unpack)
import Text.Parakeet

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
      optJInputFile = ("japanese", unpack jap)
    , optRInputFile = ("romaji", unpack rom)
    , optTemplate   = Nothing
    , optOutput     = InTex
    , optFurigana   = InHiragana
    , optNoMeta     = False
    , optKeepLV     = True
    }
    let result = case parakeet options of
                      Left a -> unlines [show a]
                      Right b -> b
    setElement (pack "result") (pack result)
  setParakeet callback
