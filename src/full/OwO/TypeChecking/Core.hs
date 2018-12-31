{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Core language
module OwO.TypeChecking.Core
  ( NameType(..)
  , ULevel(..)
  -- , BinderInfo(..)

  , Term'(..)
  , Term
  , Type
  , typeUniverseOfLevel
  , typeUniverseOfLevel'
  , definitionType

  , Definition(..)
  ) where

import qualified Data.Text            as T

import           OwO.Syntax.Abstract
import           OwO.Syntax.Common
import           OwO.Syntax.Concrete  (QModuleName (..))
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType (Name (..))

import           GHC.Generics         (Generic)

#include <impossible.h>

-- | Core language term, @i@ refers to the identifier.
--   We translate type-checked code into this form
data Term' i
  = App !(Term' i) (Term' i)
  -- ^ Application, function being applied and value applied
  | Var !Int
  -- ^ A variable resolved with de bruijn index
  | Ref NameType i Definition
  -- ^ Named reference, might be external definitions
{-
  | Bind i !(BinderInfo (Term' i)) (Term' i)
  -- ^ Name binding
-}
  | TType ULevel
  -- ^ Type of Type, including type omega
  | Const LiteralInfo
  deriving (Eq, Functor, Ord, Show)

-- TODO

-- | Term should have a @Name@ coming from the parser
type Term = Term' Name
-- | Aha! Dependent type!
type Type = Term

data Definition
  = SimpleDefinition !Type !Term
  -- ^ No pattern matching, just an expression with type known
  deriving (Eq, Ord, Show)

definitionType :: Definition -> Type
definitionType (SimpleDefinition t _) = t

-- | Built-in definition: Type0, Type1, etc
typeUniverseOfLevel :: Int -> Definition
typeUniverseOfLevel = uncurry SimpleDefinition . typeUniverseOfLevel'

typeUniverseOfLevel' :: Int -> (Term, Type)
typeUniverseOfLevel' i = (TType . ULevelLit $ succ i, TType $ ULevelLit i)
