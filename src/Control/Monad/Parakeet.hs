{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Parakeet (
  Parakeet
, runParakeet
, env
, throw
) where

import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Reader (ReaderT(..), runReaderT, asks)
import Control.Monad.Identity (Identity, runIdentity)

import Parakeet.Types.Options (Options(..))

newtype Parakeet a = Parakeet (ReaderT Options (ExceptT String Identity) a) 
  deriving ( Functor
           , Applicative
           , Monad
           )

runParakeet :: Options -> Parakeet a -> Either String a
runParakeet opts (Parakeet e) = runIdentity $ runExceptT $ runReaderT e opts

env :: (Options -> a) -> Parakeet a
env = Parakeet . asks

throw :: String -> Parakeet a
throw = Parakeet . throwError
