{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Parakeet (
  Parakeet
, runParakeet
, env
, throw

-- * Exceptions
, ParseError (..)
, TemplateError (..)
, InternalError (..)
, module Control.Exception
) where


import Control.Exception (toException, Exception, SomeException)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.Reader (ReaderT (..), runReaderT, asks)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Typeable

import Parakeet.Types.Options (Options (..))

newtype Parakeet a = Parakeet (ReaderT Options (ExceptT SomeException Identity) a) 
  deriving ( Functor
           , Applicative
           , Monad
           )
newtype ParseError = ParseError String deriving (Typeable)
newtype TemplateError = TemplateError String deriving (Typeable)
newtype InternalError = InternalError String deriving (Typeable)

instance Show ParseError where
  show (ParseError e) = "ParseError: " ++ e

instance Show TemplateError where
  show (TemplateError e) = "TemplateError: " ++ e

instance Show InternalError where
  show (InternalError e) = "InternalError: " ++ e

instance Exception ParseError
instance Exception TemplateError
instance Exception InternalError

runParakeet :: Options -> Parakeet a -> Either SomeException a
runParakeet opts (Parakeet e) = runIdentity $ runExceptT $ runReaderT e opts

env :: (Options -> a) -> Parakeet a
env = Parakeet . asks

throw :: SomeException -> Parakeet a
throw = Parakeet . throwError
