module Parser (
  parseDoc
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Control.Applicative ((<$>), (*>))
import Control.Monad (void, guard, liftM, liftM2, liftM3)
import Data.Char (toLower, toUpper, isSpace, isAlpha)
import Data.List (sortBy, nub)
import Data.Function (on)

import qualified Token.Token as T
import qualified Token.Hiragana as H
import qualified Token.Katakana as K
import qualified Token.Romaji as R
import qualified Token.Misc as M
import qualified TexElem as E

import Debug.Trace
import System.IO.Unsafe

-- * Japanese parsing

type ParserJ = Parser

hikaJ :: ParserJ T.Token
hikaJ = do
      T.Hiragana <$> liftM2 (++) hSokuon hirigana
  <|> T.Katakana <$> liftM3 cat3 kSokuon katahana choonpu
  where
    c2s c      = [c]
    cat3 a b c = a ++ b ++ c
    (&.) p1 p2 = \c -> (p1 c) && (p2 c)
    parse2 n s = do
      first <- satisfy n
      option [first] $ do 
        second <- satisfy s
        return [first, second]
    hSokuon  = option [] $ c2s <$> satisfy H.isSokuon
    kSokuon  = option [] $ c2s <$> satisfy K.isSokuon 
    hirigana = parse2 H.isNormal (H.isSmall &. (not . H.isSokuon))
    katahana = parse2 K.isNormal (K.isSmall &. (not . K.isSokuon))
    choonpu  = option [] $ c2s <$> satisfy M.isChoonpu

kanjiJ :: ParserJ T.Token
kanjiJ = T.Kanji <$> many1 (satisfy M.isKanji)

litJ :: ParserJ T.Token
litJ = T.Lit <$> many1 (satisfy other) -- many1Till?
  where 
    other c | crlf c    = False
            | otherwise = not $ or $ map (\f -> f c) 
                            [ M.isChoonpu
                            , M.isKanji, H.isHiragana, K.isKatakana ]
    crlf c = c == '\r' || c == '\n'

parseLineJ :: ParserJ [T.Token]
parseLineJ = do
  r <- many (choice [hikaJ, kanjiJ, litJ])
  void (endOfLine) <|> eof
  return $ r ++ [T.Lit "\n"]

parseJ :: ParserJ [T.Token]
parseJ = concat <$> manyTill parseLineJ eof

-- * Romaji parsing

type ParserR = Parsec String [T.Token]

removeSpace :: String -> String
removeSpace = concat . map (\c -> if isSpace c then [] else [c])

pureSpace :: String -> Bool
pureSpace = all isSpace

lowerCase :: String -> String
lowerCase = map toLower

parserCons :: Char -> ParserR ()
parserCons c = void $ do
  s <- getParserState
  setParserState $ s {
    stateInput = (:) c (stateInput s)
  }

parserPopUserToken :: ParserR T.Token
parserPopUserToken = do
  s <- getState
  guard $ not $ null s
  let token = head s
  modifyState tail
  return token

hikaR :: (T.Token -> Bool) -> (T.Token -> [T.Token]) -> (String -> String -> E.TexElem) -> ParserR [E.TexElem]
hikaR checkToken lookupToken buildTexElem = do
  token <- parserPopUserToken
  guard $ checkToken token
  let ros = map T.unwrapToken $ lookupToken token
  guard $ not (null ros)
  choice $ map (genParser token) ros
    where 
      genParser token r = string (init r) >> (noMacron r <|> hasMacron r)
        where
          noMacron r = do
            char (last r)
            (:) (buildTexElem (T.unwrapToken token) r) `liftM` parseR
          hasMacron r = do
              ch <- satisfy M.isMacron
              let no = M.noMacron ch
              let vl | no == 'o' = ['o', 'u']  -- ambiguous ō
                     | otherwise = [no]
              choice $ flip map vl $ \to -> try $ do
                parserCons to
                (:) (buildTexElem (T.unwrapToken token) r) `liftM` parseR

hiraganaR :: ParserR [E.TexElem]
hiraganaR = hikaR T.isHiraganaToken H.lookup E.Hiragana

katakanaR :: ParserR [E.TexElem]
katakanaR = hikaR T.isKatakanaToken K.lookup E.Katakana

litR :: ParserR [E.TexElem]
litR = do
  token <- parserPopUserToken
  guard $ T.isLitToken token
  let unwrapped = T.unwrapToken token
  if unwrapped == "\n"
    then spaces >> (:) E.Line `liftM` parseR
    else do
      matchIgnoreSpace (removeSpace unwrapped)
      (:) (E.Lit unwrapped) `liftM` parseR
      where
        matchIgnoreSpace []     = return ()
        matchIgnoreSpace (x:xs) = do
          spaces
          char $ toLower x -- already lowercased
          matchIgnoreSpace xs

romajiR :: ParserR T.Token
romajiR = fmap T.Romaji $ choice $ flip map romajis $ \tokens -> try (string tokens)
  where
    romajis = reverse $ nub $ sortBy (compare `on` length) $ map T.unwrapToken $ do 
      r <- R.chlst
      g <- [R.geminate, id]
      v <- [R.longVowelize True, id]
      if R.isSyllabicN r
        then return r
        else return $ g (v r)

kanjiR :: ParserR [E.TexElem]
kanjiR = do
  token <- parserPopUserToken
  guard $ T.isKanjiToken token
  let unwrapped = T.unwrapToken token
  let len = length unwrapped
  let tryRange = [1 .. len * 3 + 4]
  choice $ flip map tryRange $ \n -> try $ do
    romajis <- skip n
    let flat = concatMap T.unwrapToken romajis
    (:) (E.Kanji unwrapped [] flat) `liftM` parseR
  where
    skip n = count n (spaces >> romajiR)

breakR :: ParserR [E.TexElem]
breakR = do
  many1 space
  (:) E.Break `liftM` parseR

terminate :: ParserR ()
terminate = do
  eof
  s <- getState
  guard $ null s
  return ()

parseR :: ParserR [E.TexElem]
parseR = hiraganaR <|> katakanaR <|> kanjiR <|> litR <|> breakR <|> (terminate *> (return []))

{- spaceR :: Parser T.Token -}
{- spaceR = fmap T.Lit $ many1 $ satisfy (not . isAlpha) -}
{- test = runParser (many1 (romajiR <|> spaceR)) () [] "gokigen na chou ni natte kirameku kaze ni notte" -}

-- * Document parsing

parseDoc :: String -> String -> [E.TexElem]
parseDoc j r = evil `seq` fromEither $ runParser parseR wds [] (lowerCase r)
  where
    fromEither (Left err) = error $ show err
    fromEither (Right va) = va
    wds = fromEither (parse parseJ [] j)
    evil = (unsafePerformIO . putStrLn . concatMap (\token -> (T.unwrapToken token) ++ " ")) wds
