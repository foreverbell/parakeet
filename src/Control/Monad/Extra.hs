module Control.Monad.Extra (
  concatM
) where

import Control.Monad (liftM2)
import Data.List (foldl')

concatM :: Monad m => [m [a]] -> m [a]
concatM xs = foldl' (liftM2 (++)) (return []) xs
