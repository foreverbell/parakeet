module Control.Monad.Choice (
  Choice
, fromList
, fromList'
, toList
, fromMaybe
, toMaybe
, strip
, foremost
, rest
, ambiguous
, choose
) where

import Control.Monad (liftM, ap, MonadPlus(..))
import Control.Applicative (Alternative(..))
import Data.List (nub)

data Choice a = Choice a [a] | NoChoice
  deriving (Show)

instance Functor Choice where
  fmap = liftM
 
instance Applicative Choice where
  pure  = return
  (<*>) = ap

instance Monad Choice where
  return = flip Choice []
  NoChoice >>= _ = NoChoice
  (Choice x xs) >>= f = fromList' m $ r ++ (xs >>= toList . f)
    where m = foremost $ f x
          r = rest $ f x

instance Alternative Choice where
  (<|>) = mplus
  empty = mzero

instance MonadPlus Choice where
  mzero = NoChoice
  NoChoice `mplus` c = c
  c `mplus` _ = c
 
instance Monoid (Choice a) where
  mempty = NoChoice
  mappend x y = fromList $ toList x ++ toList y

fromList :: [a] -> Choice a
fromList [] = NoChoice
fromList xs = Choice (head xs) (tail xs)

fromList' :: a -> [a] -> Choice a
fromList' x xs = fromList (x : xs)

fromMaybe :: Maybe a -> Choice a
fromMaybe Nothing = NoChoice
fromMaybe (Just x) = Choice x []

toList :: Choice a -> [a]
toList NoChoice = []
toList (Choice x xs) = x : xs

toMaybe :: Choice a -> Maybe a
toMaybe NoChoice = Nothing
toMaybe (Choice x _) = Just x

strip :: Eq a => Choice a -> Choice a
strip NoChoice = NoChoice
strip (Choice x xs) = Choice x ys 
  where ys = nub $ filter (/= x) xs

foremost :: Choice a -> a
foremost NoChoice = error "foremost: NoChoice"
foremost (Choice x _) = x

rest :: Choice a -> [a]
rest NoChoice = error "rest: NoChoice"
rest (Choice _ xs) = xs

ambiguous :: Eq a => Choice a -> Bool
ambiguous NoChoice = False
ambiguous c = not $ null xs
  where Choice _ xs = strip c

choose :: b -> (Choice a -> b) -> Choice a -> b
choose n _ NoChoice = n
choose _ f x = f x
