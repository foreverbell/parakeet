{-# LANGUAGE ExistentialQuantification #-}

module Parakeet.Parser.Stage1 (
  stage1
) where

import           Control.Monad (forM_, void, mzero, msum, guard, replicateM)
import           Control.Monad.Choice (foremost, toList, strip)
import           Control.Monad.Parakeet
import           Data.Char (toLower, isSpace, chr)
import           Data.Char.Fuzzy (fuzzyEq, canonicalOrd)
import           Data.List (sortBy, nub, intercalate)
import           Data.Function (on)
import           Data.Maybe (fromJust)
import           Prelude hiding (break)
import           Text.Parsec
import           Text.Parsec.Extra (prepend, popUserToken)

import qualified Parakeet.Types.Token as T
import qualified Parakeet.Types.Lexeme as L
import           Parakeet.Linguistics.Hiragana ()
import           Parakeet.Linguistics.Katakana ()
import qualified Parakeet.Linguistics.Romaji as R
import qualified Parakeet.Linguistics.Misc as M

type Parser = ParsecT String [L.SomeLexeme] Parakeet
type Token = T.Token L.Single

separator :: Parser ()
separator = void $ try $ do
  spaces
  char M.separator
  notFollowedBy (char M.separator)

continue :: Token -> Parser [Token]
continue e = do
  skipMany separator
  rest <- stage1
  return $ e : rest

break :: Parser [Token]
break = do
  many1 space
  skipMany separator
  stage1

sugarize :: Bool -> Bool -> [L.Romaji L.Single] -> [L.Romaji L.Bundle]
sugarize sokuonize longVowelize from = sortBy (flip compare `on` (length . L.unwrap)) $ nub $ map L.concat $ do 
  r <- from
  g <- getTransformer sokuonize (R.sokuonize, return)
  v <- getTransformer longVowelize (R.longVowelizeWithMacron, map L.toRB)
  if R.isSyllabicN r
    then [[L.toRB r]]
    else v <$> toList (g [r])
  where
    getTransformer True (x, y)  = [x, y]
    getTransformer False (_, y) = [y]

unitRomajis :: [L.Romaji L.Bundle]
unitRomajis = sugarize True True R.chList

romaji :: [L.Romaji L.Bundle] -> Parser (L.Romaji L.Bundle)
romaji rs = L.wrap <$> choice (map (try . fuzzy) rs')
        <?> show (length rs) ++ " romaji token(s) namely (" ++ intercalate ", " rs' ++ ")"
  where 
    rs' = map L.unwrap rs
    fuzzy :: String -> Parser String
    fuzzy s = return s <* forM_ s match
      where 
        match :: Char -> Parser Char
        match c | M.isMacron c = choice $ map char (toList $ M.toMacron' c)
                | otherwise    = char c

kana :: (L.LexemeKana k) => (k -> [L.Romaji L.Single] -> Token) -> k -> Parser [Token]
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

hiragana :: L.Hiragana -> Parser [Token]
hiragana = kana T.Hiragana

katakana :: L.Katakana -> Parser [Token]
katakana = kana T.Katakana

lit :: L.Lit -> Parser [Token]
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
            then return M.separator <* string (replicate 2 M.separator) -- two separators as lit to distinguish from separator
            else fuzzyChar (toLower x)                                  -- romaji input is already lower-cased
          eat xs

fuzzyChar :: Char -> Parser Char
fuzzyChar ch = satisfy (fuzzyEq ch) <?> errorMsg 
  where
    fuzzyC = chr $ canonicalOrd ch
    errorMsg | fuzzyC == ch = "character namely \'" ++ [ch] ++ "\'" 
             | otherwise = "characters namely \'" ++ [ch] ++ "\' or \'" ++ [fuzzyC] ++ "\'"

skip :: Int -> Parser [L.Romaji L.Single]
skip n = replicateM n $ do
  void (char '-') <|> void spaces <?> "delimiter namely (\'-\' or spaces)" -- eat possible delimiters
  next <- strip . R.factor <$> romaji unitRomajis
  let (r:rs) = foremost next -- TODO: ambiguity!
  prepend $ concatMap L.unwrap rs
  return $ R.normSyllabicN r

tries :: Int -> [Int]
tries n = [1 .. n * 3 + 8]

furigana :: [L.Romaji L.Single] -> Parser ([L.Hiragana], [L.Katakana])
furigana romajis = do
  hiraganas <- maybe mzero return $ sequence (L.fromRomaji romajis :: [Maybe L.Hiragana])
  katakanas <- maybe mzero return $ sequence (L.fromRomaji romajis :: [Maybe L.Katakana])
  return (hiraganas, katakanas)

kanji :: L.Kanji -> Parser [Token]
kanji token = do
  let len = length $ L.unwrap token
  choice $ flip map (tries len) $ \n -> try $ do  
    romajis <- skip n
    (hiraganas, katakanas) <- furigana romajis
    continue $ T.Kanji token hiraganas katakanas romajis

-- | two strategies: lit matching, or fall backs to correspond romaji matching (similar to kanji).
alphanum :: L.AlphaNum -> Parser [Token]
alphanum token = try strat1 <|> strat2  
  where
    eat :: String -> Parser ()
    eat [] = return ()
    eat (x:xs) = spaces >> fuzzyChar (toLower x) >> eat xs
    strat1 = do
      eat (L.unwrap token)
      continue $ T.AlphaNum token Nothing
    strat2 = do
      let len = length $ L.unwrap token
      choice $ flip map (tries len) $ \n -> try $ do
        romajis <- skip n
        (hiraganas, katakanas) <- furigana romajis
        continue $ T.AlphaNum token $ Just (hiraganas, katakanas, romajis)

terminate :: Parser [Token]
terminate = do
  s <- getState
  guard $ null s
  eof
  return []

stage1 :: Parser [Token]
stage1 = terminate
     <|> break
     <|> do token <- popUserToken
            fromJust $ msum [ hiragana <$> L.fromLexeme token
                            , katakana <$> L.fromLexeme token
                            , kanji    <$> L.fromLexeme token
                            , lit      <$> L.fromLexeme token
                            , alphanum <$> L.fromLexeme token
                            ]
