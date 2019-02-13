{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Core language
module OwO.TypeChecking.Core
  ( BinderInfo'(..)

  , Term'(..)
  , Term
  , Type
  , typeUniverseOfLevel
  , typeUniverseOfLevel'
  , typeOfLevel
  , definitionType

  , Definition(..)
  ) where

import qualified Data.Text            as T

import           OwO.Syntax.Abstract
import           OwO.Syntax.Concrete  (LiteralInfo (..), QModuleName (..))
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType (Name (..))

#include <impossible.h>

-- | Core language term, @i@ refers to the identifier.
--   We translate type-checked code into this form
data Term' i
  = App !(Term' i) (Term' i)
  -- ^ Application, function being applied and value applied
  | Var !Int
  -- ^ A variable resolved with de bruijn index
  | Ref i Definition
  -- ^ Named reference, might be external definitions
  | Bind i !(BinderInfo' Term' i) (Term' i)
  -- ^ Name binding
  | TType (ULevel' (Term' i))
  -- ^ Type of Type, including type omega
  | Meta i
  -- ^ Meta variable
  | Const LiteralInfo
  deriving (Eq, Ord, Show)

-- TODO
data BinderInfo' t c
  = Telescope (t c)
  -- ^ Pi type binding, with a type
  | Lambda
  -- ^ Lambda abstraction (do we need a type expression?)
  deriving (Eq, Ord, Show)

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
typeUniverseOfLevel' i = (typeOfLevel $ succ i, typeOfLevel i)

typeOfLevel :: Int -> Term
typeOfLevel = TType . ULevelLit
