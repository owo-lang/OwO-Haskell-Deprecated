{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskell       #-}
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
  -- Free variable info

  , AstImplInfo'(..)
  , AstImplInfo
  -- Function body info

  , AstDeclaration'(..)
  , AstDeclaration
  -- Declarations

  , concreteToAbstractTerm
  , concreteToAbstractDecl
  , concreteToAbstractTerm'
  , concreteToAbstractDecl'
  -- Concrete to Abstract
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.List            (partition)
import qualified Data.Map             as Map
import           Each

import           OwO.Syntax.Common
import           OwO.Syntax.Concrete
import           OwO.Syntax.Context
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType (hideName, Name (..))
import qualified OwO.Util.StrictMaybe as Strict
import           OwO.Util.Three

import           GHC.Generics         (Generic)

#include <impossible.h>

-- | @a@ is @Name@ or something
data AstTerm' c
  = AstConst Loc ConstInfo
  -- ^ Constants, same as in Psi
  | AstApp (AstTerm' c) (AstTerm' c)
  -- ^ Application
  | AstBind (AstBinderInfo' AstTerm' c) (AstTerm' c)
  -- ^ Name binding
  | AstRef c (AstDeclaration' AstTerm' c)
  -- ^ A resolved reference, to a global declaration
  | AstLocalRef c (AstBinderInfo' AstTerm' c)
  -- ^ A resolved reference, to a locally binded free variable.
  | AstMetaVar c
  -- ^ Goals? Holes?
  deriving (Eq, Ord, Show)

data AstBinderInfo' t c = AstBinderInfo
  { binderName :: c
  , binderType :: t c
  } deriving (Eq, Ord, Show)

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
type AstBinderInfo  = AstBinderInfo' AstTerm' Name
type AstConsInfo    = AstConsInfo' AstTerm' Name
type AstImplInfo    = AstImplInfo' AstTerm' Name

type AstContext     = Context AstDeclaration
type TypeSignature  = (Name, FnPragmas, AstTerm)

data DesugarError
  = NoImplementationError [TypeSignature]
  -- ^ Only type signature, not implementation
  | DuplicateTypeSignatureError (TypeSignature, TypeSignature)
  -- ^ Two type signatures, with same name
  | UnresolvedReference Name
  -- ^ Usage of undefined names
  | DesugarSyntaxError String
  -- ^ Invalid syntax, but allowed by parser, disallowed by desugarer
  deriving (Eq, Ord, Show)

-- | Generate a meta var that does not present in user code
--   from a name, because we need location for the metavar anyway
inventMetaVar :: Name -> AstTerm
inventMetaVar = AstMetaVar . hideName

-- | Generate a binder with a name only
inventBinder :: Name -> AstBinderInfo
inventBinder name = AstBinderInfo
  { binderName = name
  , binderType = inventMetaVar name
  }

concreteToAbstractDecl :: Either DesugarError AstContext
concreteToAbstractDecl = concreteToAbstractDecl' emptyCtx [] []

concreteToAbstractTerm :: PsiTerm -> Either DesugarError AstTerm
concreteToAbstractTerm = concreteToAbstractTerm' emptyCtx Map.empty

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
        , (~! concreteToAbstractTerm' env Map.empty sig)
        ) : sigs
      , env
      ) |])
    desugar (PsiImplementation name pgms clauses) =
      case partition ((== name) . fst3) sigs of
        ([sig], rest) -> desugarFunction sig rest
        ([   ], rest) -> desugarFunction (inventMetaVar name) rest
        (a : b : _,_) -> Left $ DuplicateTypeSignatureError (a, b)
      where
        -- TODO deal with pragmas
        desugarFunction ty rest = __TODO__
    desugar decl = __TODO__

concreteToAbstractTerm'
  :: AstContext
  -> Binding AstBinderInfo
  -- Local variables
  -> PsiTerm
  -- Input term
  -> Either DesugarError AstTerm
concreteToAbstractTerm' env localEnv =
  \case
    (PsiReference  name) -> case Map.lookup name localEnv of
      Just ref -> Right $ AstLocalRef name ref
      Nothing  -> case lookupCtxCurrent name env of
        Just ref -> Right $ AstRef name ref
        Nothing  -> Left $ UnresolvedReference name
    (PsiLambda var body) ->
      let binder   = inventBinder var
          newLocal = Map.insert var binder localEnv
      in $(each [|
        AstBind binder
        (~! recurEnv newLocal body) |])
    (PsiApplication f a) -> $(each [| AstApp (~! recur f) (~! recur a) |])
    (PsiConstant  loc t) -> Right $ AstConst loc t
    (PsiImpossible  loc) -> __TODO__
    (PsiInaccessible  t) -> __TODO__
    (PsiMetaVar    name) -> Right $ AstMetaVar name
    (PsiTelescope var vis ty val) -> __TODO__
  where
    recur = concreteToAbstractTerm' env localEnv
    recurEnv = concreteToAbstractTerm' env
