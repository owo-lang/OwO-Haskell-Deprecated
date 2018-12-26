{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

-- | Abstract syntax tree (see @OwO.Syntax.Concrete@)
--   Prefixed with "Ast", stands for "Abstract Syntax Tree"
module OwO.Syntax.Abstract
  ( Visibility(..)

  -- Expressions
  , AstTerm'(..)
  , AstTerm

  -- Constructor info
  , AstConsInfo'(..)
  , AstConsInfo

  -- Function body info
  , AstImplInfo'(..)
  , AstImplInfo

  -- Declarations
  , AstDeclaration'(..)
  , AstDeclaration

  -- Concrete to Abstract
  , concreteToAbstractTerm
  , concreteToAbstractDecl
  , concreteToAbstractTerm'
  , concreteToAbstractDecl'
  ) where

import           Control.Applicative
import           Control.Monad
import           OwO.Syntax.Common
import           OwO.Syntax.Concrete
import           OwO.Syntax.Context
import           OwO.Syntax.Position
import qualified OwO.Util.StrictMaybe as Strict

import           GHC.Generics         (Generic)

#include <impossible.h>

-- | All parameters becomes explicit in Ast
--   this stores the explicit/implicit information of Cst
data Visibility
  = Explicit
  | Implicit
  | Instance
  deriving (Eq, Ord)

instance Show Visibility where
  show Explicit = "(   )"
  show Implicit = "{   }"
  show Instance = "{| |}"

-- | @a@ is @C.Name@ or something
data AstTerm' c
  = AstConst Loc ConstInfo
  | AstRef c (AstDeclaration' AstTerm' c)
  | AstMetaVar Loc
  deriving (Eq, Ord, Show)

-- | Type constructors, data constructors
--   construct expressions under normal form
data AstConsInfo' t c = AstConsInfo
  { consName      :: c
  , consLoc       :: Loc
  , consSignature :: t c
  } deriving (Eq, Ord, Show)

data AstImplInfo' t c = AstImplInfo
  { implName :: c
  , implLoc  :: Loc
  , implType :: t c
  , implBody :: Strict.Maybe (t c)
  } deriving (Eq, Ord, Show)

-- | @a@ is @C.Name@ or something
data AstDeclaration' t c
  = AstTypeCons c (t c) [AstConsInfo' t c]
  -- ^ Type Constructors must be declared with a list of Data Constructors
  | AstDataCons (AstConsInfo' t c)
  -- ^ Data Constructors must be declared within a Type Constructor
  | AstImplementation (AstImplInfo' t c)
  -- ^ Function implementation, type, implementation
  deriving (Eq, Ord, Show)

type AstDeclaration = AstDeclaration' AstTerm' Name
type AstTerm        = AstTerm' Name
type AstConsInfo    = AstConsInfo' AstTerm' Name
type AstImplInfo    = AstImplInfo' AstTerm' Name

type AstContext     = Context AstDeclaration

concreteToAbstractDecl :: [PsiDeclaration] -> (AstContext, [AstDeclaration])
concreteToAbstractDecl = concreteToAbstractDecl' emptyCtx

concreteToAbstractTerm :: PsiTerm -> AstTerm
concreteToAbstractTerm = concreteToAbstractTerm' emptyCtx emptyCtx

concreteToAbstractDecl' :: AstContext -> [PsiDeclaration] -> (AstContext, [AstDeclaration])
concreteToAbstractDecl' env [      ] = (env, [])
concreteToAbstractDecl' env (d : ds) =
    let (decl, newEnv) = desugar d
    in (decl :) <$> checkRest newEnv
  where
    checkRest = flip concreteToAbstractDecl' ds
    desugar decl = __TODO__

concreteToAbstractTerm' :: AstContext -> Context AstTerm -> PsiTerm -> AstTerm
concreteToAbstractTerm' env localEnv = \case
  _ -> __TODO__
