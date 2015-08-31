{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad.Parakeet (
  Parakeet
, runParakeet
) where

import Control.Applicative (Applicative(..))
import Control.Monad.Error (ErrorT(..), runErrorT, MonadError(..))
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader.Class (MonadReader(..))

import Options (Options(..))

newtype Parakeet a = Parakeet (ReaderT Options (ErrorT String Identity) a) 
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Options
           , MonadError String )

runParakeet :: Options -> Parakeet a -> Either String a
runParakeet opts (Parakeet e) = runIdentity $ runErrorT $ runReaderT e opts