module Eval (
  Eval
, runEval
) where

import Control.Monad.Error (ErrorT(..), runErrorT)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Identity (Identity, runIdentity)

import Options (Options(..))

type Eval = ReaderT Options (ErrorT String Identity)

runEval :: Options -> Eval a -> Either String a
runEval opts e = runIdentity $ runErrorT $ runReaderT e opts

