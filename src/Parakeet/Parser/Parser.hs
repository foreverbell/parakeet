module Parakeet.Parser.Parser (
  ParsedDocument
, parse
) where

import           Control.Monad (liftM)
import           Control.Monad.Parakeet (Parakeet, env, throw, toException, ParseError (..))
import           Data.Char (isSpace)
import           Data.Char.Extra (toLower)
import           Data.List (isPrefixOf)
import           Text.Parsec hiding (parse)

import           Parakeet.Types.FToken
import           Parakeet.Types.Meta
import           Parakeet.Types.Options
import           Parakeet.Parser.Stage0 (stage0)
import           Parakeet.Parser.Stage1 (stage1)
import           Parakeet.Parser.Stage2 (stage2)
import qualified Parakeet.Parser.WithLine as L

type ParsedDocument = (Maybe Meta, [FToken])

setLine l = do
  pos <- getPosition
  setPosition $ setSourceLine pos l

parseLine :: (Line, Line, String, String) -> Parakeet [FToken]
parseLine (lj, lr, j, r) = do
  jName <- fst <$> env optJInputFile
  rName <- fst <$> env optRInputFile
  wd <- fromEither =<< runParserT (setLine lj >> stage0) () jName j
  tk <- fromEither =<< runParserT (setLine lr >> stage1) wd rName (toLower r)
  keeplv <- env optKeepLV
  furigana <- env optFurigana
  if keeplv
     then liftM (concatLit . map (fromToken furigana)) (stage2 tk)
     else return $ concatLit $ map (fromToken furigana) tk
  where
    fromEither = either (throw . toException . ParseError . show) return

formatMeta :: (String, String) -> Maybe (String, String) -> Parakeet Meta
formatMeta (j1, j2) (Just (r1, r2)) = do
  title <- if null r1 then return [Lit j1] else init <$> parseLine (1, 1, j1, r1)
  author <- if null r2 then return [Lit j2] else init <$> parseLine (2, 2, j2, r2)
  let authorLit = if null r2 then [Lit j2] else [Lit j2, Lit ("(" ++ r2 ++ ")")]
  return $ Meta (Title title, Author (author, authorLit))
formatMeta (j1, j2) Nothing = return $ Meta (Title [Lit j1], Author ([Lit j2], [Lit j2]))

parse :: Parakeet ParsedDocument
parse = do
  jContent <- snd <$> env optJInputFile
  rContent <- snd <$> env optRInputFile
  let j@(js, _) = L.hstrip (L.create $ lines jContent)
  let r@(rs, _) = L.hstrip (L.create $ lines rContent)
  let (js0, js1, rs0, rs1) = (js!!0, js!!1, rs!!0, rs!!1)
  ignoreMeta <- env optNoMeta
  let jHasMeta = not ignoreMeta && hasMeta js
  let rHasMeta = not ignoreMeta && jHasMeta && hasMeta rs 
  let j' | jHasMeta = L.hstrip $ L.drop 2 j
         | otherwise = j
  let r' | rHasMeta = L.hstrip $ L.drop 2 r
         | otherwise = r
  meta <- if jHasMeta
    then do
      let jSection = (getMetaData js0, getMetaData js1)
      let rSection | rHasMeta  = Just (getMetaData rs0, getMetaData rs1)
                   | otherwise = Nothing
      Just <$> formatMeta jSection rSection
    else return Nothing
  token <- concat <$> mapM parseLine (L.zip2 j' r')
  return (meta, token)
  where 
    hasMeta (a:b:_) = isMetaLine a && isMetaLine b
    hasMeta _       = False
    isMetaLine l = "##" `isPrefixOf` l 
    getMetaData l = dropWhile isSpace $ drop 2 l
