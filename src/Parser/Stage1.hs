module Parser.Stage1 (
  stage1
) where

import           Text.Parsec
import           Control.Applicative ((<$>), (*>), (<*))
import           Control.Monad (void, guard, replicateM, liftM)
import           Data.Char (toLower, isSpace)
import           Data.List (sortBy, nub)
import           Data.Function (on)
import           Data.Maybe (fromJust, isJust)
import           Prelude hiding (break)

import qualified Token.Token as T
import qualified Token.Hiragana as H
import qualified Token.Katakana as K
import qualified Token.Romaji as R
import qualified Token.Misc as M
import qualified Element as E

type Parser = Parsec String [T.Token]

removeSpace :: String -> String
removeSpace = concatMap (\c -> if isSpace c then [] else [c])

parserPrepend :: String -> Parser ()
parserPrepend a = void $ do
  s <- getParserState
  setParserState $ s {
    stateInput = (++) a (stateInput s)
  }

parserPopUserToken :: Parser T.Token
parserPopUserToken = do
  s <- getState
  guard $ not $ null s
  let token = head s
  modifyState tail
  return token

hika :: (T.Token -> Bool) -> (T.Token -> [[T.Token]]) -> (String -> [String] -> E.Element) -> Parser [E.Element]
hika checkTokenType lookupToken buildElement = do
  token <- parserPopUserToken
  guard $ checkTokenType token
  let romajis = map (map T.unwrapToken) (lookupToken token)
  choice $ map (gen token) romajis
    where 
      gen token r = perfect r <|> withMacron1 r <|> withMacron2 r
        where
          perfect r = try $ string (concat r) >> cont token r
          withMacron1 r = try $ do -- (me, mē), split ē to ee
            let cr = concat r
            string $ init cr
            ch <- satisfy M.isMacron
            let un = M.unMacron ch
            guard $ un == last cr
            let vl | un == 'o' = ['o', 'u']  -- ambiguous 'ō'
                   | otherwise = [un]
            choice $ flip map vl $ \to -> try $ parserPrepend [to] >> cont token r
          withMacron2 r = try $ do -- (mee, mē)
            let cr = concat r
            let l = length cr
            guard $ l >= 2
            string $ take (l - 2) cr
            ch <- satisfy M.isMacron
            let un = M.unMacron ch
            guard $ replicate 2 un == drop (l - 2) cr
            cont token r
      cont token r = do
        let r' = map T.unwrapToken $ concatMap (R.normalize . T.Romaji) r
        (:) (buildElement (T.unwrapToken token) r') `liftM` stage1

hiragana :: Parser [E.Element]
hiragana = hika T.isHiraganaToken H.fromHiragana E.Hiragana

katakana :: Parser [E.Element]
katakana = hika T.isKatakanaToken K.fromKatakana E.Katakana

lit :: Parser [E.Element]
lit = do
  token <- parserPopUserToken
  guard $ T.isLitToken token
  let unwrapped = T.unwrapToken token
  if unwrapped == "\n"
    then return [E.Line] <* (spaces >> eof)
    else do
      matchIgnoreSpace (removeSpace unwrapped)
      (:) (E.Lit unwrapped) `liftM` stage1
      where
        matchIgnoreSpace []     = return ()
        matchIgnoreSpace (x:xs) = do
          spaces
          char $ toLower x -- Romaji input is already lower-cased
          matchIgnoreSpace xs

romaji :: Parser T.Token
romaji = fmap T.Romaji $ choice $ flip map romajis $ \tokens -> try (string tokens)
  where
    romajis = reverse $ nub $ sortBy (compare `on` length) $ map (concatMap T.unwrapToken) $ do 
      r <- R.chlst
      g <- [R.sokuonize, id]
      v <- [R.longVowelize True, id]
      if R.isSyllabicN r
        then return [r]
        else return $ g (v [r])

kanji :: Parser [E.Element]
kanji = do
  token <- parserPopUserToken
  guard $ T.isKanjiToken token
  let unwrapped = T.unwrapToken token
  let len = length unwrapped
  let tryRange = [1 .. len * 3 + 8]
  choice $ flip map tryRange $ \n -> try $ do
    romajis <- skip n
    let hiraganas = H.toHiragana romajis
    guard $ isJust hiraganas
    (:) (E.Kanji unwrapped (flatten (fromJust hiraganas)) (flatten romajis)) `liftM` stage1
  where
    skip n = replicateM n $ do
      spaces
      r <- R.normalize <$> (spaces >> romaji)
      parserPrepend $ concatMap T.unwrapToken (tail r)
      return $ head r
    flatten = map T.unwrapToken

break :: Parser [E.Element]
break = do
  many1 space
  (:) E.Break `liftM` stage1

terminate :: Parser ()
terminate = do
  eof
  s <- getState
  guard $ null s
  return ()

stage1 :: Parser [E.Element]
stage1 = hiragana <|> katakana <|> kanji <|> lit <|> break <|> (terminate *> return [])

{- space' :: Parser T.Token -}
{- space' = fmap T.Lit $ many1 $ satisfy (not . isAlpha) -}
{- test = runParser (many1 (romaji <|> space')) () [] "gokigen na chou ni natte kirameku kaze ni notte" -}
