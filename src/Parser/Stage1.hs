{-# LANGUAGE ExistentialQuantification #-}

module Parser.Stage1 (
  stage1
, TokenBox(..)
) where

import           Text.Parsec
import           Control.Monad (void, guard, mzero, replicateM)
import           Control.Monad.Trans (lift)
import           Control.Monad.Reader (asks)
import           Data.Char (toLower, isSpace)
import           Data.List (sortBy, nub)
import           Data.Function (on)
import           Prelude hiding (break)

import qualified Token.Token as T
import qualified Token.Compound as C
import           Token.Hiragana ()
import           Token.Katakana ()
import qualified Token.Romaji as R
import           Monad.Choice (foremost, toList, strip)
import           Monad.Parakeet
import           Options (Options(..), FuriganaFormat(..))

type Parser = ParsecT String [TokenBox] Parakeet

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
  p <- getPosition
  setParserState $ s {
    stateInput = (++) a (stateInput s)
  }
  setPosition $ incSourceColumn p (negate $ length a)

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

vary :: Bool -> Bool -> [T.Romaji] -> [T.Romaji]
vary sokuonize longVowelize from = reverse $ sortBy (compare `on` length . T.unwrap) $ nub $ map mconcat $ do 
  r <- from
  g <- set sokuonize [R.sokuonize, id]
  v <- set longVowelize [R.longVowelize True, id]
  return $ if R.isSyllabicN r
    then [r]
    else g (v [r])
  where set True xs  = xs
        set False xs = drop 1 xs

varities :: [T.Romaji]
varities = vary True True R.chlst

romaji :: [T.Romaji] -> Parser T.Romaji
romaji = fmap T.wrap . choice . map (try . string) . map T.unwrap

kana :: (T.TokenKana k) => k -> Parser [C.Compound]
kana token = choice $ map (match token) (toList $ T.toRomaji token)
  where
    match token romajis = try $ go romajis
      where
        curElement = T.buildCompound token $ map R.normalizeSyllabicN romajis
        go :: [T.Romaji] -> Parser [C.Compound]
        go [] = continue curElement
        go (r:rs) = do
          next <- toList . strip . R.normalize <$> romaji (vary False True $ toList (R.otherForms r))
          choice $ flip map next $ \rlist -> try $ do
            prepend $ concatMap T.unwrap (tail rlist)
            guard $ (head rlist) == r
            go rs

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
        matchIgnoreSpace :: String -> Parser ()
        matchIgnoreSpace []     = return ()
        matchIgnoreSpace (x:xs) = do
          spaces
          char $ toLower x -- Romaji input is already lower-cased
          matchIgnoreSpace xs
        removeSpace = filter (not . isSpace)

kanji :: T.Kanji -> Parser [C.Compound]
kanji token = do
  let unwrapped = T.unwrap token
  let len = length unwrapped
  let tryRange = [1 .. len * 3 + 8]
  furigana <- lift $ asks optFurigana
  choice $ flip map tryRange $ \n -> try $ do  
    romajis <- fmap T.unwrap <$> skip n
    kanas <- case furigana of
               InKatakana -> maybe mzero kFlatten $ T.fromNRomaji (T.wrap <$> romajis)
               _          -> maybe mzero hFlatten $ T.fromNRomaji (T.wrap <$> romajis)
    continue $ C.Kanji unwrapped kanas romajis
  where
    hFlatten hs = return $ map T.unwrap (hs :: [T.Hiragana])
    kFlatten ks = return $ map T.unwrap (ks :: [T.Katakana])
    skip n = replicateM n $ do
      spaces
      next <- strip . R.normalize <$> romaji varities
      let (r:rs) = foremost next
      prepend $ concatMap T.unwrap rs
      return r

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
     <|> do TokenBox token <- popUserToken
            match token
