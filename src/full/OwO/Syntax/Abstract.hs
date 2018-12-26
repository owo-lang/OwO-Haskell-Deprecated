{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import           Data.List            (partition)
import           Each

import           OwO.Syntax.Common
import           OwO.Syntax.Concrete
import           OwO.Syntax.Context
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType (Name(..))
import qualified OwO.Util.StrictMaybe as Strict
import           OwO.Util.Three

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
type TypeSignature  = (Name, FnPragmas, AstTerm)

data DesugarError
  = NoImplementationError [TypeSignature]
  -- ^ Only type signature, not implementation
  | DuplicateTypeSignatureError (TypeSignature, TypeSignature)
  -- ^ Two type signatures, with same name
  deriving (Eq, Ord, Show)

concreteToAbstractDecl :: Either DesugarError AstContext
concreteToAbstractDecl = concreteToAbstractDecl' emptyCtx [] []

concreteToAbstractTerm :: PsiTerm -> Either DesugarError AstTerm
concreteToAbstractTerm = concreteToAbstractTerm' emptyCtx emptyCtx

concreteToAbstractDecl'
  :: AstContext
  -- Existing context
  -> [TypeSignature]
  -- Unimplemented declarations
  -> [PsiDeclaration]
  -- Unchecked declarations
  -> Either DesugarError AstContext
concreteToAbstractDecl' env [] [      ] = Right env
concreteToAbstractDecl' env sb [      ] = Left $ NoImplementationError sb
concreteToAbstractDecl' env sigs (d : ds) = do
    (newSigs, newEnv) <- desugar d
    checkRest newEnv newSigs
  where
    checkRest env sig = concreteToAbstractDecl' env sig ds
    desugar (PsiTypeSignature name pgms sig) = $(each [|
      ( ( name
        , pgms
        , (~! concreteToAbstractTerm' env emptyCtx sig)
        ) : sigs
      , env
      ) |])
    desugar (PsiImplementation name pgms clauses) = case partition ((== name) . fst3) sigs of
        ([sig], rest) -> __TODO__
        ([   ], rest) -> __TODO__
        ((s0 : s1 : _), _) -> Left $ DuplicateTypeSignatureError (s0, s1)
      where

    desugar decl = __TODO__

concreteToAbstractTerm'
  :: AstContext
  -> Context AstTerm
  -- Local variables
  -> PsiTerm
  -- Input term
  -> Either DesugarError AstTerm
concreteToAbstractTerm' env localEnv term
  = __TODO__
