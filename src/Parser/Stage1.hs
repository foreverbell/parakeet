{-# LANGUAGE ExistentialQuantification #-}

module Parser.Stage1 (
  stage1
, TokenBox(..)
) where

import           Text.Parsec
import           Control.Monad (forM_, void, guard, mzero, replicateM)
import           Control.Monad.Trans (lift)
import           Control.Monad.Reader (asks)
import           Data.Char (toLower, isSpace)
import           Data.List (sortBy, nub, intercalate)
import           Data.Function (on)
import           Prelude hiding (break)

import qualified Token.Token as T
import qualified Token.Compound as C
import           Token.Hiragana ()
import           Token.Katakana ()
import qualified Token.Romaji as R
import qualified Token.Misc as M
import           Monad.Choice (Choice, foremost, toList, strip, choose)
import           Monad.Parakeet
import           Parser.FuzzyChar (fuzzyEq)
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

instance TokenCompoundable T.Separator where
  match = separator

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

sugarize :: Bool -> Bool -> [T.Romaji] -> [T.Romaji]
sugarize sokuonize longVowelize from = reverse . sortBy (compare `on` length . T.unwrap) $ nub $ map mconcat $ do 
  r <- from
  g <- set sokuonize [R.sokuonize, id]
  v <- set longVowelize [R.longVowelize True, id]
  return $ if R.isSyllabicN r
    then [r]
    else v (g [r])
  where set True xs  = xs
        set False xs = drop 1 xs

allSugarized :: [T.Romaji]
allSugarized = sugarize True True R.chList

romaji :: [T.Romaji] -> Parser T.Romaji
romaji rs = T.wrap <$> (choice $ map (try . fuzzy) rs')
        <?> show (length rs) ++ " romaji token(s) namely (" ++ intercalate ", " rs' ++ ")"
  where 
    rs' = map T.unwrap rs
    fuzzy :: String -> Parser String
    fuzzy s = return s <* do
      forM_ s $ \c -> do
        if M.isMacron c
          then let (a, b) = M.toMacron $ M.unMacron c in char a <|> char b
          else char c

kana :: (T.TokenKana k) => k -> Parser [C.Compound]
kana token = choice $ go <$> (toList $ T.toRomaji token)
  where
    go romajis = try $ rec romajis
      where
        curElement = T.buildCompound token $ map R.normSyllabicN romajis
        rec :: [T.Romaji] -> Parser [C.Compound]
        rec [] = continue curElement
        rec (r:rs) = do
          next <- toList . strip . R.cut <$> romaji (sugarize False True [r])
          choice $ flip map next $ \rlist -> try $ do
            prepend $ concatMap T.unwrap (tail rlist)
            guard $ (head rlist) == r
            rec rs

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
          if M.isSeparator x
            then return M.separator <* string (replicate 2 M.separator)
            else char' $ toLower x -- Romaji input is already lower-cased
          matchIgnoreSpace xs
        removeSpace = filter (not . isSpace)
        char' :: Char -> Parser Char
        char' ch = satisfy (fuzzyEq ch) <?> "character likely \'" ++ [ch, '\''] 

kanji :: T.Kanji -> Parser [C.Compound]
kanji token = do
  let unwrapped = T.unwrap token
  let len = length unwrapped
  let tryRange = [1 .. len * 3 + 8]
  furigana <- lift $ asks optFurigana
  choice $ flip map tryRange $ \n -> try $ do  
    romajis <- fmap T.unwrap <$> skip n
    kanas <- sequence $ case furigana of
               InKatakana -> map (choose mzero flatten) (T.fromRomaji (T.wrap <$> romajis) :: [Choice T.Katakana])
               _          -> map (choose mzero flatten) (T.fromRomaji (T.wrap <$> romajis) :: [Choice T.Hiragana])
    continue $ C.Kanji unwrapped kanas romajis
  where
    -- TODO: warn something if ambiguous
    flatten :: (T.TokenKana k) => Choice k -> Parser String
    flatten = return . T.unwrap . foremost
    skip n = replicateM n $ do
      void (char '-') <|> void spaces <?> "delimiter likely (\'-\' or spaces)" -- eat possible delimiters
      next <- strip . R.cut <$> romaji allSugarized
      let (r:rs) = foremost next
      prepend $ concatMap T.unwrap rs
      return $ R.normSyllabicN r

separator :: T.Separator -> Parser [C.Compound]
separator = const $ do
  spaces
  char M.separator
  stage1

break :: Parser [C.Compound]
break = do
  many1 space
  continue C.Break

terminate :: Parser [C.Compound]
terminate = do
  s <- getState
  guard $ null s
  eof
  return []

stage1 :: Parser [C.Compound]
stage1 = terminate
     <|> break
     <|> do TokenBox token <- popUserToken
            match token
