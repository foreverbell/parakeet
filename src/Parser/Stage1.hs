{-# LANGUAGE ExistentialQuantification #-}

module Parser.Stage1 (
  stage1
, AnyToken(..)
, TokenWrap
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

type Parser = Parsec String [AnyToken]

class T.Token t => TokenWrap t where
  match :: t -> Parser [E.Element]
 
data AnyToken = forall t. TokenWrap t => AnyToken t

instance TokenWrap T.Hiragana where
  match = hiragana
  
instance TokenWrap T.Katakana where
  match = katakana

instance TokenWrap T.Kanji where
  match = kanji

instance TokenWrap T.Lit where
  match = lit

removeSpace :: String -> String
removeSpace = filter (not . isSpace)

prepend :: String -> Parser ()
prepend a = void $ do
  s <- getParserState
  setParserState $ s {
    stateInput = (++) a (stateInput s)
  }

popUserToken :: Parser AnyToken
popUserToken = do
  s <- getState
  guard $ not $ null s
  let token = head s
  modifyState tail
  return token

continue :: E.Element -> Parser [E.Element]
continue e = (:) e `liftM` stage1

hika :: (T.Token t) => t -> (t -> [[T.Romaji]]) -> (String -> [String] -> E.Element) -> Parser [E.Element]
hika token lookupToken buildElement = do
  let romajis = lookupToken token
  choice $ map (generate token) romajis
    where 
      generate token romaji = perfect 
                          <|> withMacron1
                          <|> withMacron2
        where
          catRomaji = concat $ map T.unwrap romaji
          lenRomaji = length catRomaji
          plnRomaji = map T.unwrap $ concatMap R.normalize romaji
          curElement = buildElement (T.unwrap token) plnRomaji 
          perfect = try $ do
            string catRomaji
            continue curElement
          withMacron1 = try $ do -- (me, mē), split ē to ee
            string $ init catRomaji
            ch <- satisfy M.isMacron
            let un = M.unMacron ch
            guard $ un == last catRomaji
            let vl | un == 'o' = ['o', 'u']  -- ambiguous 'ō'
                   | otherwise = [un]
            choice $ flip map vl $ \to -> try $ do
              prepend [to]
              continue curElement
          withMacron2 = try $ do -- (mee, mē)
            guard $ lenRomaji >= 2
            string $ take (lenRomaji - 2) catRomaji
            ch <- satisfy M.isMacron
            let un = M.unMacron ch
            guard $ replicate 2 un == drop (lenRomaji - 2) catRomaji
            continue curElement

hiragana :: T.Hiragana -> Parser [E.Element]
hiragana token = hika token H.fromHiragana E.Hiragana

katakana :: T.Katakana -> Parser [E.Element]
katakana token = hika token K.fromKatakana E.Katakana

lit :: T.Lit -> Parser [E.Element]
lit token = do
  let unwrapped = T.unwrap token
  if unwrapped == "\n"
    then return [E.Line] <* (spaces >> eof)
    else do
      matchIgnoreSpace $ removeSpace unwrapped
      continue $ E.Lit unwrapped
      where
        matchIgnoreSpace []     = return ()
        matchIgnoreSpace (x:xs) = do
          spaces
          char $ toLower x -- Romaji input is already lower-cased
          matchIgnoreSpace xs

expectRomajis :: [String]
expectRomajis = reverse $ nub $ sortBy (compare `on` length) $ map (concatMap T.unwrap) $ do 
  r <- R.chlst
  g <- [R.sokuonize, id]
  v <- [R.longVowelize True, id]
  if R.isSyllabicN r
    then return [r]
    else return $ g (v [r])

romaji :: Parser T.Romaji
romaji = fmap T.wrap $ choice $ map (\token -> try (string token)) expectRomajis

kanji :: T.Kanji -> Parser [E.Element]
kanji token = do
  let unwrapped = T.unwrap token
  let len = length unwrapped
  let tryRange = [1 .. len * 3 + 8]
  choice $ flip map tryRange $ \n -> try $ do
    romajis <- skip n
    let hiraganas = H.toHiragana romajis
    guard $ isJust hiraganas
    let unwrappedH = map T.unwrap $ fromJust hiraganas
    let unwrappedR = map T.unwrap romajis  
    continue $ E.Kanji unwrapped unwrappedH unwrappedR
  where
    skip n = replicateM n $ do
      spaces
      r <- R.normalize <$> (spaces >> romaji)
      prepend $ concatMap T.unwrap (tail r)
      return $ head r

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
stage1 = (terminate *> return [])
     <|> break
     <|> do
           AnyToken token <- popUserToken
           match token
  
