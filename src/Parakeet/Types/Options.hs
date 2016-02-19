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
  inputFileJ   :: File
, inputFileR   :: File
, templateFile :: Maybe File
, furigana     :: FuriganaFormat
, noMeta       :: Bool
, keepLV       :: Bool
}
