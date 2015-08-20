{-# LANGUAGE OverloadedStrings #-}

module Print (
  prettyPrint
) where

import qualified Data.Text.Lazy as T

import           Options (Options(..))
import           Template (template)
import           Parser.Parser (doParse)
import qualified Element as E

prettyPrint :: Options -> String -> String -> String
prettyPrint opts j r = if (optNoWrap opts)
  then T.unpack $ T.concat [hder, "\n\n", body]
  else T.unpack $ T.unlines $ flip map tmpl $ \t -> 
    case t of
      "$body$" -> body
      _        -> t 
  where
    hder = "% automatically generated by obtuse-parakeet"
    tmpl = hder : map (T.filter (/= '\r')) (T.lines template)
    body = E.texify $ doParse opts j r