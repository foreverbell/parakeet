{-# LANGUAGE ExistentialQuantification #-}

module Parser.Stage1 (
  stage1
, TokenBox(..)
) where

import           Text.Parsec
import           Control.Applicative ((<$>), (*>), (<*))
import           Control.Monad (void, guard, replicateM)
import           Data.Char (toLower, isSpace)
import           Data.List (sortBy, nub)
import           Data.Function (on)
import           Data.Maybe (fromJust, isJust)
import           Prelude hiding (break)

import qualified Token.Token as T
import qualified Token.Compound as C
import           Token.Hiragana ()
import           Token.Katakana ()
import qualified Token.Romaji as R
import qualified Token.Misc as M

type Parser = Parsec String [TokenBox]

class T.Token t => TokenCompoundable t where
  match :: t -> Parser [C.Compound]
 
data TokenBox = forall t. TokenCompoundable t => TokenBox t

instance TokenCompoundable T.Hiragana where
  match = hiragana
  
instance TokenCompoundable T.Katakana where
  match = katakana

instance TokenCompoundable T.Kanji where
  match = kanji

instance TokenCompoundable T.Lit where
  match = lit

prepend :: String -> Parser ()
prepend a = void $ do
  s <- getParserState
  setParserState $ s {
    stateInput = (++) a (stateInput s)
  }

popUserToken :: Parser TokenBox
popUserToken = do
  s <- getState
  guard $ not $ null s
  let token = head s
  modifyState tail
  return token

continue :: C.Compound -> Parser [C.Compound]
continue e = do
  rest <- stage1
  return $ e : rest

kana :: (T.TokenKana k) => k -> Parser [C.Compound]
kana token = do
  let romajis = T.toRomaji token
  choice $ map (generate token) romajis
    where 
      generate token romaji = perfect 
                          <|> withMacron1
                          <|> withMacron2
        where
          catRomaji = concat $ map T.unwrap romaji
          lenRomaji = length catRomaji
          curElement = T.buildCompound token $ concatMap R.normalize romaji 
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

hiragana :: T.Hiragana -> Parser [C.Compound]
hiragana token = kana token

katakana :: T.Katakana -> Parser [C.Compound]
katakana token = kana token

lit :: T.Lit -> Parser [C.Compound]
lit token = do
  let unwrapped = T.unwrap token
  if unwrapped == "\n"
    then return [C.Line] <* (spaces >> eof)
    else do
      matchIgnoreSpace $ removeSpace unwrapped
      continue $ C.Lit unwrapped
      where
        matchIgnoreSpace []     = return ()
        matchIgnoreSpace (x:xs) = do
          spaces
          char $ toLower x -- Romaji input is already lower-cased
          matchIgnoreSpace xs
        removeSpace = filter (not . isSpace)

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

kanji :: T.Kanji -> Parser [C.Compound]
kanji token = do
  let unwrapped = T.unwrap token
  let len = length unwrapped
  let tryRange = [1 .. len * 3 + 8]
  choice $ flip map tryRange $ \n -> try $ do
    romajis <- skip n
    let hiraganas = T.fromNRomaji romajis :: Maybe [T.Hiragana]
    guard $ isJust hiraganas
    let unwrappedH = map T.unwrap $ fromJust hiraganas
    let unwrappedR = map T.unwrap romajis  
    continue $ C.Kanji unwrapped unwrappedH unwrappedR
  where
    skip n = replicateM n $ do
      spaces
      r <- R.normalize <$> (spaces >> romaji)
      prepend $ concatMap T.unwrap (tail r)
      return $ head r

break :: Parser [C.Compound]
break = do
  many1 space
  continue C.Break

terminate :: Parser ()
terminate = do
  eof
  s <- getState
  guard $ null s
  return ()

stage1 :: Parser [C.Compound]
stage1 = (terminate *> return [])
     <|> break
     <|> do
           TokenBox token <- popUserToken
           match token

