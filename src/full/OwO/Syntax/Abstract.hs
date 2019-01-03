{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE TypeOperators         #-}

-- | Abstract syntax tree (see @OwO.Syntax.Concrete@)
--   Prefixed with "Ast", stands for "Abstract Syntax Tree"
module OwO.Syntax.Abstract
  ( AstTerm'(..)
  , AstTerm
  , inventMetaVar
  -- Expressions

  , AstConsInfo'(..)
  , AstConsInfo
  -- Constructor info

  , AstBinderInfo'(..)
  , AstBinderInfo
  , inventBinder
  , AstBinderKind'(..)
  , AstBinderKind
  , binderValue
  -- Free variable info

  , AstImplInfo'(..)
  , AstImplInfo
  -- Function body info

  , AstDeclaration'(..)
  , AstDeclaration
  -- Declarations

  , ULevel(..)
  ) where

import           Control.Applicative
import           Control.Monad

import           OwO.Syntax.Common
import           OwO.Syntax.Concrete
import           OwO.Syntax.Context
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType (Name (..), hideName)
import qualified OwO.Util.StrictMaybe as Strict

#include <impossible.h>

-- | @a@ is @Name@ or something
data AstTerm' c
  = AstLiteral Loc LiteralInfo
  -- ^ Constants, same as in Psi
  | AstTypeLit c ULevel
  -- ^ Type literal
  | AstApp (AstTerm' c) (AstTerm' c)
  -- ^ Application
  | AstBind (AstBinderInfo' AstTerm' c) (AstTerm' c)
  -- ^ Name binding
  | AstRef c (AstDeclaration' AstTerm' c)
  -- ^ A resolved reference, to a global declaration
  | AstLocalRef c (AstBinderInfo' AstTerm' c)
  -- ^ A resolved reference, to a locally bound free variable.
  | AstMetaVar c
  -- ^ Goals? Holes?
  deriving (Eq, Ord, Show)

data AstBinderInfo' t c = AstBinderInfo
  { binderName :: c
  , binderType :: t c
  , binderKind :: AstBinderKind' (t c)
  } deriving (Eq, Ord, Show)

-- | i should be something like a @Term@
data AstBinderKind' t
  = LambdaBinder
  -- ^ Lambda abstraction
  | TelescopeBinder Visibility
  -- ^ Pi type's binding
  | LetBinder t
  -- ^ Let binding
  | GeneratedBinder t
  -- ^ Intermediate value binding generated
  deriving (Eq, Ord, Show)

binderValue :: AstBinderKind' a -> Maybe a
binderValue  LambdaBinder       = Nothing
binderValue (TelescopeBinder _) = Nothing
binderValue (LetBinder value)   = Just value
binderValue (GeneratedBinder v) = Just v

-- | Type constructors, data constructors
--   construct expressions under normal form
data AstConsInfo' t c = AstConsInfo
  { consName      :: c
  , consSignature :: t c
  } deriving (Eq, Ord, Show)

data AstImplInfo' t c = AstImplInfo
  { implName :: c
  , implType :: t c
  , implBody :: t c
  } deriving (Eq, Ord, Show)

-- | @a@ is @C.Name@ or something
data AstDeclaration' t c
  = AstTypeCons c (t c) [AstConsInfo' t c]
  -- ^ Type Constructors must be declared with a list of Data Constructors
  | AstDataCons (AstConsInfo' t c)
  -- ^ Data Constructors must be declared within a Type Constructor
  | AstImplementation (AstImplInfo' t c)
  -- ^ Function implementation, type, implementation
  | AstPostulate c (t c)
  -- ^ Functions with types but no implementations
  deriving (Eq, Ord, Show)

data ULevel
  = ULevelLit Int
  -- ^ Like Type0, Type1
  | ULevelVar String Int
  -- ^ Level variables. Should be already computed.
  | ULevelMax
  -- ^ TypeInf, TypeOmega
  deriving (Eq, Ord, Show)

type AstDeclaration = AstDeclaration' AstTerm' Name
type AstTerm        = AstTerm' Name
type AstBinderInfo  = AstBinderInfo' AstTerm' Name
type AstBinderKind  = AstBinderKind' AstTerm
type AstConsInfo    = AstConsInfo' AstTerm' Name
type AstImplInfo    = AstImplInfo' AstTerm' Name

-- | Generate a meta var that does not present in user code
--   from a name, because we need location for the metavar anyway
inventMetaVar :: Name -> AstTerm
inventMetaVar = AstMetaVar . hideName

-- | Generate a binder with a name only
inventBinder :: Name -> AstBinderKind -> AstBinderInfo
inventBinder name kind = AstBinderInfo
  { binderName = name
  , binderType = inventMetaVar name
  , binderKind = kind
  }
