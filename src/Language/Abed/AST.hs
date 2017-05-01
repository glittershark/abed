{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}

module Language.Abed.AST
    -- ( Expr(..)
    -- , Val(..) , ValExpr
    -- , Equ(..) , EquExpr
    -- )
    where

import Control.Lens hiding (uncons, snoc, parts)
import Data.Comp
import Data.Comp.Derive

data Expr f = In (f (Expr f))

data Namespace = Namespace { _finalPart :: Text
                           , _parts     :: Vector Text }
                           deriving (Eq, Generic, Hashable)

parts :: Namespace -> Vector Text
parts (Namespace final parts) = snoc parts final

instance Show Namespace where
    show = unpack . intercalate "." . parts

instance Hashable a => Hashable (NonNull a) where
  hashWithSalt salt = hashWithSalt salt . toNullable

data Ident = Ident { identNamespace :: Maybe Text
                   , identName      :: NonNull Text
                   } deriving (Show, Eq, Generic, Hashable)

simpleIdent :: NonNull Text -> Ident
simpleIdent = Ident Nothing

data Val e = IntVal { _getInt :: Int }
           | StrVal { _getStr :: Text }
           | SumVal { _getSum :: (Val e, Val e) }
           deriving (Show, Eq)
makeLenses ''Val
makePrisms ''Val

asInt :: Prism' (Val e) Int
asInt = prism IntVal extract
  where extract (IntVal x) = Right x
        extract v          = Left v

asStr :: Prism' (Val e) Text
asStr = prism StrVal extract
  where extract (StrVal x) = Right x
        extract v          = Left v

asSum :: Prism' (Val e) (Val e, Val e)
asSum = prism SumVal extract
  where extract (SumVal x) = Right x
        extract v          = Left v

type ValExpr = Expr Val

data Equ e = Eq e e
type EquExpr = Expr Equ

data Invoke e = Invoke { _invokeIdent :: e
                       , _invokeArg   :: e
                       } deriving (Show, Eq, Generic)
makeFields ''Invoke
type InvokeExpr = Expr Invoke

data Macroinvoke e = Macroinvoke { _macroinvokeIdent :: Ident
                                 , _macroinvokeBody  :: e
                                 } deriving (Show, Eq, Generic)
makeFields ''Macroinvoke
type MacroinvokeExpr = Expr Macroinvoke

data CondBranch e = CondBranch { _condBranchPredicate :: e
                               , _condBranchBody      :: e
                               } deriving (Show, Eq, Generic)

data SpecialForm e = Binding { _bindingIdent :: Ident
                             , _bindingExpr  :: e
                             , _bindingBody  :: e
                             }
                   | Cond    { _condBranches :: [CondBranch e]
                             }
                   | Lambda  { _lambdaBindings :: [Ident]
                             , _lambdaBody     :: e
                             , _lambdaIsMacro  :: Bool
                             }
                   | Quote   { _quoteExpr :: e }
                   deriving (Eq, Show, Generic)
makeFields ''SpecialForm
makePrisms ''SpecialForm

data Block e = Block { _blockBody :: Vector e }
makeFields ''Block
type BlockExpr = Expr Block

type Sig =  Val
        :+: Equ
        :+: Invoke
        :+: Macroinvoke
        :+: SpecialForm
        :+: Block

derive [ makeFunctor, makeEqF, makeShowF, smartConstructors
       , smartAConstructors ]
       [''Val, ''Equ, ''Invoke, ''Macroinvoke, ''Block]

