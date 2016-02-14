module Parakeet.Parser.WithLine (
  WithLine
, create
, drop
, hstrip
, zip2
) where

import           Data.Char (isSpace)
import           Data.List (zip4)
import           Prelude hiding (drop)
import qualified Prelude as P

type WithLine = ([String], Int)

create :: [String] -> WithLine
create buf = (buf, 1)

drop :: Int -> WithLine -> WithLine
drop d (buf, l) = (P.drop d buf, l + d)

hstrip :: WithLine -> WithLine
hstrip wl@(buf, _) = drop emptys wl
  where 
    emptys = length $ takeWhile isEmpty buf
    isEmpty = not . any (not . isSpace)

zip2 :: WithLine -> WithLine -> [(Int, Int, String, String)]
zip2 (b1, l1) (b2, l2) = zip4 [l1 .. ] [l2 .. ] b1 b2
