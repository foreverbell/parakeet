module Text.Parsec.Extra ( 
  setLine
, prepend
, popUserToken
) where

import Control.Monad (void, guard)
import Text.Parsec

setLine :: Monad m => Line -> ParsecT s u m ()
setLine l = do
  pos <- getPosition
  setPosition $ setSourceLine pos l

prepend :: Monad m => String -> ParsecT String u m ()
prepend a = void $ do
  s <- getParserState
  p <- getPosition
  setParserState $ s {
    stateInput = (++) a (stateInput s)
  }
  setPosition $ incSourceColumn p (negate $ length a)

popUserToken :: Monad m => ParsecT s [u] m u
popUserToken = do
  s <- getState
  guard $ not $ null s
  modifyState tail
  return $ head s
