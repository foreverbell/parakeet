module Parser.Parser (
  parse
) where

import Text.Parsec hiding (parse)
import Control.Monad.Reader (asks)
import Control.Monad.Except (throwError)
import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf, zipWith4)

import Monad.Parakeet (Parakeet)
import Options (Options(..))
import Parser.Stage0 (stage0)
import Parser.Stage1 (stage1)
import Parser.FlatToken (FlatToken(..), flatten)
import Parser.MetaInfo (MetaInfo(..), Author(..), Title(..))

setLine l = do
  pos <- getPosition
  setPosition $ setSourceLine pos l

parseLine :: Line -> Line -> String -> String -> Parakeet [FlatToken]
parseLine lj lr j r = do
  jf <- asks optJInputFile
  rf <- asks optRInputFile
  wd <- test =<< runParserT (setLine lj >> stage0) () jf j
  tk <- test =<< runParserT (setLine lr >> stage1) wd rf (map toLower r)
  sequence $ fmap flatten tk
  where test = either (throwError . show) return

extractMetaInfo :: (String, String) -> Maybe (String, String) -> Parakeet MetaInfo
extractMetaInfo (j1, j2) (Just (r1, r2)) = do
  title <- if null r1 then return [Lit j1] else init <$> parseLine 1 1 j1 r1
  author <- if null r2 then return [Lit j2] else init <$> parseLine 2 2 j2 r2
  let authorLit = if null r2 then [Lit j2] else [Lit j2, Lit ("(" ++ r2 ++ ")")]
  return $ MetaInfo (Title title, Author (author, authorLit))
extractMetaInfo (j1, j2) Nothing = return $ MetaInfo (Title [Lit j1], Author ([Lit j2], [Lit j2]))

trimFront :: ([String], Line) -> ([String], Line)
trimFront (ls, l) = (drop emptys ls, l + emptys)
  where 
    emptys = length $ takeWhile isEmpty ls
    isEmpty = not . any (not . isSpace)

-- TODO: refactor `parse` in a monadic way
parse :: Parakeet (Maybe MetaInfo, [FlatToken])
parse = do
  (j, r) <- asks optContent
  let (js@(js0:js1:_), offsetJ) = trimFront (lines j, 1)
  let (rs@(rs0:rs1:_), offsetR) = trimFront (lines r, 1)
  ignoreMeta <- asks optNoMetaInfo
  let hasMetaJ = not ignoreMeta && hasMeta js
  let hasMetaR = not ignoreMeta && hasMetaJ && hasMeta rs 
  let (js', offsetJ') = trimFront $ if hasMetaJ then (drop 2 js, offsetJ + 2) else (js, offsetJ)
  let (rs', offsetR') = trimFront $ if hasMetaR then (drop 2 rs, offsetR + 2) else (rs, offsetR)
  metaInfo <- if hasMetaJ
    then if hasMetaR
      then Just <$> extractMetaInfo (metaData js0, metaData js1) (Just (metaData rs0, metaData rs1))
      else Just <$> extractMetaInfo (metaData js0, metaData js1) Nothing
    else return Nothing
  tokens <- concat <$> sequence (zipWith4 parseLine [offsetJ' .. ] [offsetR' .. ] js' rs')
  return (metaInfo, tokens)
  where hasMeta f = case f of
          (a:b:_) -> isMetaLine a && isMetaLine b
          _       -> False
        isMetaLine l = "##" `isPrefixOf` l
        metaData l = dropWhile isSpace $ drop 2 l
