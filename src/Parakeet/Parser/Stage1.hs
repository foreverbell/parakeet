{-# LANGUAGE ExistentialQuantification #-}

module Parakeet.Parser.Stage1 (
  stage1
, TokenBox(..)
) where

import           Text.Parsec
import           Control.Monad (forM_, void, mzero, guard, replicateM)
import           Control.Monad.Choice (foremost, toList, strip)
import           Control.Monad.Parakeet
import           Data.Char (toLower, isSpace)
import           Data.Char.Fuzzy (fuzzyEq)
import           Data.List (sortBy, nub, intercalate)
import           Data.Function (on)
import           Prelude hiding (break)

import qualified Parakeet.Linguistics.Lexeme as L
import           Parakeet.Linguistics.Hiragana ()
import           Parakeet.Linguistics.Katakana ()
import qualified Parakeet.Linguistics.Romaji as R
import qualified Parakeet.Linguistics.Misc as M
import qualified Parakeet.Parser.Token as T

type Parser = ParsecT String [TokenBox] Parakeet

class L.Lexeme t => TokenType t where
  match :: t -> Parser [T.Token]

data TokenBox = forall t. (TokenType t, Show t) => TokenBox t

instance TokenType L.Hiragana where
  match = hiragana

instance TokenType L.Katakana where
  match = katakana

instance TokenType L.Kanji where
  match = kanji

instance TokenType L.Lit where
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

separator :: Parser ()
separator = void $ try $ do
  spaces
  char M.separator
  notFollowedBy (char M.separator)

continue :: T.Token -> Parser [T.Token]
continue e = do
  skipMany separator
  rest <- stage1
  return $ e : rest

sugarize :: Bool -> Bool -> [L.Romaji] -> [L.Romaji]
sugarize sokuonize longVowelize from = sortBy (flip compare `on` (length . L.unwrap)) $ nub $ map mconcat $ do 
  r <- from
  g <- set sokuonize [R.sokuonize, id]
  v <- set longVowelize [R.longVowelize True, id]
  return $ if R.isSyllabicN r
    then [r]
    else v (g [r])
  where set True xs  = xs
        set False xs = drop 1 xs

romaji :: [L.Romaji] -> Parser L.Romaji
romaji rs = L.wrap <$> choice (map (try . fuzzy) rs')
        <?> show (length rs) ++ " romaji token(s) namely (" ++ intercalate ", " rs' ++ ")"
  where 
    rs' = map L.unwrap rs
    fuzzy :: String -> Parser String
    fuzzy s = return s <* forM_ s match
      where 
        match :: Char -> Parser Char
        match c = if M.isMacron c
                        then let (a, b) = M.toMacron $ M.unMacron c in char a <|> char b
                        else char c

kana :: (L.LexemeKana k) => (k -> [L.Romaji] -> T.Token) -> k -> Parser [T.Token]
kana builder token = choice $ go <$> toList (L.toRomaji token)
  where
    go romajis = try $ iter romajis
      where
        curElement = builder token $ map R.normSyllabicN romajis
        iter [] = continue curElement
        iter (r:rs) = do
          next <- toList . strip . R.factor <$> romaji (sugarize False True [r])
          choice $ flip map next $ \rlist -> try $ do
            prepend $ concatMap L.unwrap (tail rlist)
            guard $ head rlist == r
            iter rs

hiragana :: L.Hiragana -> Parser [T.Token]
hiragana = kana T.Hiragana

katakana :: L.Katakana -> Parser [T.Token]
katakana = kana T.Katakana

lit :: L.Lit -> Parser [T.Token]
lit token = do
  let unwrapped = L.unwrap token
  if unwrapped == "\n"
    then return [T.Line] <* (spaces >> eof)
    else do
      eat $ filter (not . isSpace) unwrapped
      continue $ T.Lit token
      where
        eat :: String -> Parser ()
        eat []     = return ()
        eat (x:xs) = do
          spaces
          if M.isSeparator x
            then return M.separator <* string (replicate 2 M.separator) -- Two separators as lit to distinguish from separator
            else char' $ toLower x -- Romaji input is already lower-cased
          eat xs
        char' :: Char -> Parser Char
        char' ch = satisfy (fuzzyEq ch) <?> "character likely \'" ++ [ch, '\''] 

kanji :: L.Kanji -> Parser [T.Token]
kanji token = do
  let len = length $ L.unwrap token
  let tryRange = [1 .. len * 3 + 8]
  choice $ flip map tryRange $ \n -> try $ do  
    romajis <- skip n
    hiraganas <- maybe mzero return $ sequence (L.fromRomaji romajis :: [Maybe L.Hiragana])
    katakanas <- maybe mzero return $ sequence (L.fromRomaji romajis :: [Maybe L.Katakana])
    continue $ T.Kanji token hiraganas katakanas romajis
  where
    skip n = replicateM n $ do
      void (char '-') <|> void spaces <?> "delimiter likely (\'-\' or spaces)" -- eat possible delimiters
      next <- strip . R.factor <$> romaji unitRomajis
      let (r:rs) = foremost next -- TODO: ambiguity!
      prepend $ concatMap L.unwrap rs
      return $ R.normSyllabicN r

unitRomajis :: [L.Romaji]
unitRomajis = sugarize True True R.chList

break :: Parser [T.Token]
break = do
  many1 space
  continue T.Break

terminate :: Parser [T.Token]
terminate = do
  s <- getState
  guard $ null s
  eof
  return []

stage1 :: Parser [T.Token]
stage1 = terminate
     <|> break
     <|> do TokenBox token <- popUserToken
            match token
