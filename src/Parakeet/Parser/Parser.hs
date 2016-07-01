module Parakeet.Parser.Parser (
  parse
) where

import           Control.Monad (liftM)
import           Control.Monad.Parakeet (Parakeet, env, throw, toException, ParseError (..))
import           Data.Char (isSpace)
import           Data.Char.Extra (toLower)
import           Data.List (isPrefixOf)
import           Text.Parsec hiding (parse)
import           Text.Parsec.Extra (setLine)

import           Parakeet.Types.FToken
import           Parakeet.Types.Document
import qualified Parakeet.Types.Options as O
import           Parakeet.Parser.Stage0 (stage0)
import           Parakeet.Parser.Stage1 (stage1)
import           Parakeet.Parser.Stage2 (stage2)
import qualified Parakeet.Parser.WithLine as L

parseLine :: (Line, Line, String, String) -> Parakeet [FToken]
parseLine (lj, lr, j, r) = do
  jName <- fst <$> env O.inputFileJ
  rName <- fst <$> env O.inputFileR
  wd <- fromEither =<< runParserT (setLine lj >> stage0) () jName j
  tk <- fromEither =<< runParserT (setLine lr >> stage1) wd rName (toLower r)
  keepLV <- env O.keepLV
  furigana <- env O.furigana
  if keepLV
    then liftM (concatLit . map (fromToken furigana)) (stage2 tk)
    else return $ concatLit $ map (fromToken furigana) tk
  where
    fromEither = either (throw . toException . ParseError . show) return

parse :: Parakeet Document
parse = do
  jContent <- snd <$> env O.inputFileJ
  rContent <- snd <$> env O.inputFileR
  let j@(jBuffer, _) = L.create $ lines jContent
  ignoreMeta <- env O.noMeta
  let has = not ignoreMeta && length jBuffer >= 2 && all ("##" `isPrefixOf`) (take 2 jBuffer)
  let jRest | has = L.hstrip $ L.drop 2 j
            | otherwise = j
  let getMeta = dropWhile isSpace . drop 2
  let metaInfo | has = Just Meta { title = getMeta (jBuffer!!0), author = getMeta (jBuffer!!1) }
           | otherwise = Nothing
  body <- concat <$> mapM parseLine (L.zip2 jRest (L.hstrip $ L.create $ lines rContent))
  return Document { metaInfo = metaInfo, body = body }
