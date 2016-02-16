module Data.List.Extra (
  concatMapLast
) where

concatMapLast :: (a -> [a]) -> [a] -> [a]
concatMapLast _ [] = []
concatMapLast f [x] = f x
concatMapLast f (x:xs) = x : concatMapLast f xs
