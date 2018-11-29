{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Core language
module OwO.TypeChecking.Core
  ( NameType(..)
  , ULevel(..)
  , BinderInfo(..)
  , ConstInfo(..)

  , Term'(..)
  , Term
  , Type
  , typeUniverseOfLevel
  , typeUniverseModule

  , Definition(..)
  ) where

import qualified Data.Text           as T

import           OwO.Syntax.Abstract
import           OwO.Syntax.Common
import           OwO.Syntax.Position

import           GHC.Generics        (Generic)

#include <impossible.h>

data NameType
  = BoundName
  -- ^ Local name, should be already resolved.
  | FunctionName
  -- ^ Non-local name, should be already resolved.
  | TypeConstructor
  -- ^ Type constructor, should be already resolved.
  | DataConstructor
  -- ^ Data constructor, should be already resolved.
  deriving (Eq, Generic, Ord, Show)

data ULevel
  = ULevelLit Int
  -- ^ Like Type0, Type1
  | ULevelVar String Int
  -- ^ Level variables. Should be already computed.
  | ULevelMax
  -- ^ TypeInf, TypeOmega
  deriving (Eq, Generic, Ord, Show)

-- | i should be something like a @Term@
data BinderInfo i
  = LambdaBinder !i
  -- ^ Lambda abstraction, type
  {-
  | Pi !i
  -}
  | LetBinder !i i
  -- ^ Let binding, type and value
  | NLetBinder !i i
  -- ^ Intermediate value used for reduction
  deriving (Eq, Functor, Generic, Ord, Show)

-- | Core language term, @i@ refers to the identifier.
--   We translate type-checked code into this form
data Term' i
  = App !(Term' i) (Term' i)
  -- ^ Application
  | Var !Int
  -- ^ A variable resolved with de bruijn index
  | Param NameType i (Term' i)
  -- ^ Named reference, might be external definitions
  | Bind i !(BinderInfo (Term' i)) (Term' i)
  -- ^ Name binding
  | TType ULevel
  -- ^ Type of Type, including type omega
  | Constant ConstInfo
  deriving (Eq, Functor, Generic, Ord, Show)

-- TODO

-- | Term should have a @Name@ coming from the parser
type Term = Term' Name
-- | Aha! Dependent type!
type Type = Term

data Definition
  = SimpleDefinition !Type !Term
  -- ^ No type signature, just an expression with (optional) type specified
  deriving (Eq, Generic, Ord, Show)

-- | Built-in definition: Type0, Type1, etc
typeUniverseOfLevel :: Int -> Definition
typeUniverseOfLevel i =
  SimpleDefinition (TType . ULevelLit $ succ i) (TType $ ULevelLit i)

-- | Module name for type universes. Like a placeholder
typeUniverseModule :: QModuleName
typeUniverseModule = QModuleName
  { moduleNameList = T.pack <$> ["OwO", "Primitive"]
  }
