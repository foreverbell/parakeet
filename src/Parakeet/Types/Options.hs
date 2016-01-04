module Parakeet.Types.Options (
  Options(..)
, OutputFormat(..)
, FuriganaFormat(..)
) where

data OutputFormat = InTex 
                  | InBareTex 
                  | InIntermediate 
                  deriving (Eq)
data FuriganaFormat = InHiragana 
                    | InKatakana 
                    deriving (Eq)

data Options = Options {
  optContent    :: (String, String)
, optJInputFile :: FilePath
, optRInputFile :: FilePath
, optOutput     :: OutputFormat
, optFurigana   :: FuriganaFormat
, optMincho     :: String
, optGothic     :: String
, optShowBreak  :: Bool
, optNoMeta     :: Bool
, optKeepLV     :: Bool
}
