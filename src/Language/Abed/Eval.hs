{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Abed.Eval
    (
    ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Data.Comp
import Data.Comp.Derive

import Language.Abed.AST
import Language.Abed.Scope
import Language.Abed.Error

type EvalM = StateT Scope (ExceptT EvalError IO)

class Eval f v where
    evalAlg :: AlgM EvalM f (Term v)

evalM :: (Traversable f, Eval f v) => Term f -> EvalM (Term v)
evalM = cataM evalAlg

derive [liftSum] [''Eval]

instance (f :<: v) => Eval f v where
    evalAlg = return . inject

instance (Val :<: v) => Eval SpecialForm v where
    evalAlg (Binding i e b) = do
        value <- forceProject e
        val i .= Just value
        forceProject b


