module Parakeet.Types.Options (
  Options (..)
, OutputFormat (..)
, FuriganaFormat (..)
, File
) where

data OutputFormat = InTex  -- TODO: pdf
                  | InPlainTex
                  deriving (Eq)
data FuriganaFormat = InHiragana 
                    | InKatakana 
                    deriving (Eq)

type File = (FilePath, String)

data Options = Options {
  optJInputFile :: File
, optRInputFile :: File
, optTemplate   :: Maybe File
, optOutput     :: OutputFormat
, optFurigana   :: FuriganaFormat
, optNoMeta     :: Bool
, optKeepLV     :: Bool
}
