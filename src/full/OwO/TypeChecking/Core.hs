{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Core language
module OwO.TypeChecking.Core
  ( NameType(..)
  , ULevel(..)
  -- , BinderInfo(..)
  , ConstInfo(..)

  , Term'(..)
  , Term
  , Type
  , typeUniverseOfLevel
  , typeUniverseModule
  , primitiveModule

  , builtinDefinition
  , builtinDefinition'
  , definitionType

  , Definition(..)
  ) where

import           Data.Char
import qualified Data.Text            as T

import           OwO.Syntax.Abstract
import           OwO.Syntax.Common
import           OwO.Syntax.Concrete  (QModuleName (..))
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType (Name (..))

import           GHC.Generics         (Generic)

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
  deriving (Eq, Ord, Show)

data ULevel
  = ULevelLit Int
  -- ^ Like Type0, Type1
  | ULevelVar String Int
  -- ^ Level variables. Should be already computed.
  | ULevelMax
  -- ^ TypeInf, TypeOmega
  deriving (Eq, Ord, Show)

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
  | Const ConstInfo
  deriving (Eq, Functor, Ord, Show)

-- TODO

-- | Term should have a @Name@ coming from the parser
type Term = Term' Name
-- | Aha! Dependent type!
type Type = Term

data Definition
  = SimpleDefinition !Type !Term
  -- ^ No pattern matching, just an expression with (optional) type specified
  deriving (Eq, Ord, Show)

definitionType :: Definition -> Type
definitionType (SimpleDefinition t _) = t

-- | Built-in definition: Type0, Type1, etc
typeUniverseOfLevel :: Int -> Definition
typeUniverseOfLevel = uncurry SimpleDefinition . typeUniverseOfLevel'

typeUniverseOfLevel' :: Int -> (Term, Type)
typeUniverseOfLevel' i = (TType . ULevelLit $ succ i, TType $ ULevelLit i)

-- | Module name for type universes. Like a placeholder
typeUniverseModule :: QModuleName
typeUniverseModule = primitiveModule

primitiveModule :: QModuleName
primitiveModule = QModuleName
  { moduleNameList = ["OwO", "Primitive"]
  }

typeText = "Type" :: T.Text
infText = "Inf" :: T.Text
arrowText = "->" :: T.Text

-- | Because there're infinite `TypeN`s, no way of pre-putting them into a module
builtinDefinition' :: T.Text -> Maybe (Term, Type)
builtinDefinition' t
  | t `T.isPrefixOf` typeText = let trailing = T.drop 4 t in if
    | T.all isDigit trailing -> Just . typeUniverseOfLevel' . read $ T.unpack trailing
    | trailing == infText    -> Just (TType ULevelMax, TType ULevelMax)
    | otherwise              -> Nothing
  | t == arrowText = Just __TODO__
  | otherwise = Nothing

-- | Because there're infinite `TypeN`s, no way of pre-putting them into a module
builtinDefinition :: T.Text -> Maybe Definition
builtinDefinition = (uncurry SimpleDefinition <$>) . builtinDefinition'
