{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}

module Language.Abed.Scope where

import Control.Lens
import Language.Abed.AST
import Unsafe.Coerce

class HasValues s a | s -> a where
    values :: Lens' s a
    {-# MINIMAL values #-}

data ScopeVal = ScopeVal { unScopeVal :: forall e. Val e }
deriving instance Show ScopeVal

forceScopeVal :: Val e -> ScopeVal
forceScopeVal v = ScopeVal $ unsafeCoerce v -- ugh! impredicativity!

scopeVal :: Iso' ScopeVal (Val e)
scopeVal = iso unScopeVal forceScopeVal

data Scope = Scope { _scopeValues :: HashMap Ident ScopeVal }
makeFields ''Scope

sval :: Ident -> Lens' Scope (Maybe ScopeVal)
sval i = values . at i

-- val :: Ident -> Traversal' Scope (Val e)
-- val i = sval i . _Just . scopeVal
val i = lens get set
  where
    get :: Scope -> Maybe (Val e)
    get scope   = scope ^? sval i . _Just . scopeVal
    set :: Scope -> Maybe (Val e) -> Scope
    set scope v = scope & sval i .~ map forceScopeVal v

-- helper
makeValTypeLens :: Prism' (Val e) a
                -> Ident -> Lens' Scope (Maybe a)
makeValTypeLens pris i = lens get set
  where get scope = scope ^? val i . _Just . pris
        set scope v = scope & val i .~ map (view (re pris)) v

int :: Ident -> Lens' Scope (Maybe Int)
int = makeValTypeLens asInt

str :: Ident -> Lens' Scope (Maybe Text)
str = makeValTypeLens asStr

pair :: Ident -> Lens' Scope (Maybe (Val e, Val e))
pair = makeValTypeLens asSum

