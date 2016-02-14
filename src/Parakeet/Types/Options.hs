module Parakeet.Types.Options (
  Options (..)
, FuriganaFormat (..)
, File
) where

data FuriganaFormat = InHiragana 
                    | InKatakana 
                    deriving (Eq)

type File = (FilePath, String)

data Options = Options {
  optJInputFile :: File
, optRInputFile :: File
, optTemplate   :: Maybe File
, optFurigana   :: FuriganaFormat
, optNoMeta     :: Bool
, optKeepLV     :: Bool
}
