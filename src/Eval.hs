{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval (
  Eval
, runEval
) where

import Control.Applicative (Applicative(..))
import Control.Monad.Error (ErrorT(..), runErrorT)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Error (MonadError(..))

import Options (Options(..))

newtype Eval a = Eval (ReaderT Options (ErrorT String Identity) a) 
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Options
           , MonadError String )

runEval :: Options -> Eval a -> Either String a
runEval opts (Eval e) = runIdentity $ runErrorT $ runReaderT e opts

