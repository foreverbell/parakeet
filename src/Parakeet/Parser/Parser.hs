module Parakeet.Parser.Parser (
  parse
) where

import           Control.Monad.Parakeet (Parakeet, env, throw)
import           Data.Char (isSpace)
import           Data.Char.Extra (toLower)
import           Data.List (isPrefixOf, zip4)
import           Text.Parsec hiding (parse)

import           Parakeet.Types.FlatToken (FlatToken(..))
import qualified Parakeet.Types.Lexeme as Lexeme
import           Parakeet.Types.Meta (Meta(..), Author(..), Title(..))
import qualified Parakeet.Types.Token as Token
import           Parakeet.Types.Options (Options(..), FuriganaFormat(..))
import           Parakeet.Parser.Stage0 (stage0)
import           Parakeet.Parser.Stage1 (stage1)
import           Parakeet.Parser.Stage2 (stage2)

setLine l = do
  pos <- getPosition
  setPosition $ setSourceLine pos l

parseLine :: (Line, Line, String, String) -> Parakeet [FlatToken]
parseLine (lj, lr, j, r) = do
  keeplv <- env optKeepLV
  let stage2' = if keeplv then stage2 else return
  jf <- env optJInputFile
  rf <- env optRInputFile
  wd <- fromEither =<< runParserT (setLine lj >> stage0) () jf j
  tk <- stage2' =<< fromEither =<< runParserT (setLine lr >> stage1) wd rf (toLower r)
  sequence $ flatten <$> tk
  where fromEither = either (throw . show) return

formatMeta :: (String, String) -> Maybe (String, String) -> Parakeet Meta
formatMeta (j1, j2) (Just (r1, r2)) = do
  title <- if null r1 then return [Lit j1] else init <$> parseLine (1, 1, j1, r1)
  author <- if null r2 then return [Lit j2] else init <$> parseLine (2, 2, j2, r2)
  let authorLit = if null r2 then [Lit j2] else [Lit j2, Lit ("(" ++ r2 ++ ")")]
  return $ Meta (Title title, Author (author, authorLit))
formatMeta (j1, j2) Nothing = return $ Meta (Title [Lit j1], Author ([Lit j2], [Lit j2]))

type WithLine = ([String], [Line])

dropl :: Int -> WithLine -> WithLine
dropl d (buf, l) = (drop d buf, drop d l)

lstrip :: WithLine -> WithLine
lstrip wl@(buf, _) = dropl emptys wl
  where 
    emptys = length $ takeWhile isEmpty buf
    isEmpty = not . any (not . isSpace)

zipl :: WithLine -> WithLine -> [(Line, Line, String, String)]
zipl (b1, l1) (b2, l2) = zip4 l1 l2 b1 b2

flatten :: Token.Token -> Parakeet FlatToken
flatten token = 
  case token of
       Token.Line -> return Line
       Token.Break -> return Break
       Token.Lit l -> return $ Lit (Lexeme.unwrap l)
       Token.Hiragana h r -> return $ Hiragana (Lexeme.unwrap h) (map Lexeme.unwrap r)
       Token.Katakana k r -> return $ Katakana (Lexeme.unwrap k) (map Lexeme.unwrap r)
       Token.Kanji k hs ks r -> do
          let romaji = map Lexeme.unwrap r
          furigana <- env optFurigana
          let kana = case furigana of
                InKatakana -> map Lexeme.unwrap ks
                InHiragana -> map Lexeme.unwrap hs
          return $ Kanji (Lexeme.unwrap k) kana romaji

parse :: Parakeet (Maybe Meta, [FlatToken])
parse = do
  j@(js, _) <- fst <$> env optContent >>= \j -> return (lstrip (lines j, [1 .. ]))
  r@(rs, _) <- snd <$> env optContent >>= \r -> return (lstrip (lines r, [1 .. ]))
  let (js0, js1, rs0, rs1) = (js!!0, js!!1, rs!!0, rs!!1)
  ignoreMeta <- env optNoMeta
  let hasMetaJ = not ignoreMeta && hasMeta js
  let hasMetaR = not ignoreMeta && hasMetaJ && hasMeta rs 
  let j' | hasMetaJ = lstrip $ dropl 2 j
         | otherwise = j
  let r' | hasMetaR = lstrip $ dropl 2 r
         | otherwise = r
  meta <- if hasMetaJ
    then do
      let jSection = (getMetaData js0, getMetaData js1)
      let rSection | hasMetaR  = Just (getMetaData rs0, getMetaData rs1)
                   | otherwise = Nothing
      Just <$> formatMeta jSection rSection
    else return Nothing
  token <- concat <$> mapM parseLine (zipl j' r')
  return (meta, token)
  where 
    hasMeta (a:b:_) = isMetaLine a && isMetaLine b
    hasMeta _       = False
    isMetaLine l = "##" `isPrefixOf` l 
    getMetaData l = dropWhile isSpace $ drop 2 l
