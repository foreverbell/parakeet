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
, optOutputIO   :: String -> IO () -- TODO: refactor it
, optOutput     :: OutputFormat
, optFurigana   :: FuriganaFormat
, optMincho     :: String
, optGothic     :: String
, optShowBreak  :: Bool
, optNoMetaInfo :: Bool
, optKeepLV     :: Bool
}
