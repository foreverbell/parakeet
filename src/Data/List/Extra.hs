module Data.List.Extra (
  mapHead
, mapLast
) where

mapHead :: (a -> [a]) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x ++ xs

mapLast :: (a -> [a]) -> [a] -> [a]
mapLast _ [] = []
mapLast f [x] = f x
mapLast f (x:xs) = x : mapLast f xs
