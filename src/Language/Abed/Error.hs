{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Language.Abed.Error where

import Control.Monad.Except
import Data.Comp

data EvalError = StringError Text
               deriving (Show)

throwStr :: MonadError EvalError m => Text -> m a
throwStr = throwError . StringError

forceProject :: (a :<: b, MonadError EvalError m, Show (Term b))
             => Term b -> m (Term a)
forceProject x = case project x of
                   Just y -> _ y
                   Nothing -> throwStr $ "Could not project " ++ tshow x
