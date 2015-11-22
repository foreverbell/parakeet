{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Parakeet (
  Parakeet
, runParakeet
) where

import Control.Monad.Except (ExceptT(..), runExceptT, MonadError(..))
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader.Class (MonadReader(..))

import Parakeet.Types.Options (Options(..))

newtype Parakeet a = Parakeet (ReaderT Options (ExceptT String Identity) a) 
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Options
           , MonadError String )

runParakeet :: Options -> Parakeet a -> Either String a
runParakeet opts (Parakeet e) = runIdentity $ runExceptT $ runReaderT e opts
