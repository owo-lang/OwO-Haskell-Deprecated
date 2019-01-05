{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module OwO.TypeChecking.Desugar
  ( concreteToAbstractTerm
  , concreteToAbstractDecl
  , concreteToAbstractTerm'
  , concreteToAbstractDecl'
  -- Concrete to Abstract

  , AstContext
  , TypeSignature
  , DesugarError(..)
  ) where

import           Data.Functor             ((<&>))
import           Data.List                (partition)
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map                 as Map
import qualified Data.Tuple               as Pair
import           Each

import           OwO.Syntax.Abstract
import           OwO.Syntax.Concrete
import           OwO.Syntax.Context
import           OwO.Syntax.TokenType
import           OwO.TypeChecking.Builtin
import           OwO.Util.Three

#include <impossible.h>

type AstContext     = Context AstDeclaration
type TypeSignature  = (Name, FnPragmas, AstTerm)

data DesugarError
  = NoImplementationError TypeSignature
  -- ^ Only type signature, not implementation
  | DuplicatedTypeSignatureError (TypeSignature, TypeSignature)
  -- ^ Two type signatures, with same name
  | UnresolvedReferenceError Name
  -- ^ Usage of undefined names
  | DesugarSyntaxError String
  -- ^ Invalid syntax, but allowed by parser, disallowed by desugarer
  deriving (Eq, Ord, Show)

concreteToAbstractDecl :: [PsiDeclaration] -> Either DesugarError (AstContext, [DesugarError])
concreteToAbstractDecl = flip (concreteToAbstractDecl' emptyCtx []) []

concreteToAbstractTerm :: PsiTerm -> Either DesugarError AstTerm
concreteToAbstractTerm = concreteToAbstractTerm' emptyCtx Map.empty

concreteToAbstractDecl'
  :: AstContext
  -- Existing context
  -> [TypeSignature]
  -- Unimplemented declarations
  -> [PsiDeclaration]
  -- Unchecked declarations
  -> [DesugarError]
  -- Warnings (non-fatal errors)
  -> Either DesugarError (AstContext, [DesugarError])
  -- Errors, or typechecking result with warnings
concreteToAbstractDecl' env [] [      ] err = Right (env, err)
concreteToAbstractDecl' env sb [      ] err = Right (newEnv, err ++ newErr)
  where
    newDecls = sb <&> \(name, _, term) -> (name, AstPostulate name term)
    newEnv   = foldr (uncurry addDefinition) env newDecls
    newErr   = NoImplementationError <$> sb
concreteToAbstractDecl' env sigs (d : ds) err =
    desugar d >>= uncurry checkRest . Pair.swap
  where
    checkRest env sig = concreteToAbstractDecl' env sig ds err
    checkTerm = concreteToAbstractTerm' env Map.empty
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
        ([   ], rest) -> desugarFunction (name, [], inventMetaVar name) rest
        (a : b : _,_) -> Left $ DuplicatedTypeSignatureError (a, b)
      where
        -- TODO deal with pragmas
        -- Type signature, clauses
        desugarFunction (_, tyPgms, ty) rest = case clauses of
          ((PsiImplSimple name pattern [] rhs whereBlock) :| []) -> do
            body <- checkTerm rhs
            let new = AstImplementation AstImplInfo
                  { implName = name
                  , implType = ty
                  , implBody = body
                  }
            return (rest, addDefinition name new env)
          _ -> __TODO__
    -- TODO deal with pragmas
    desugar (PsiPostulate name pgms ty) = $(each [|
        ( sigs
        , addDefinition name (AstPostulate name (~! checkTerm ty)) env
        ) |])
    desugar decl = return __TODO__

concreteToAbstractTerm'
  :: AstContext
  -> Binding AstBinderInfo
  -- Local variables
  -> PsiTerm
  -- Input term
  -> Either DesugarError AstTerm
  -- Errors, or typechecking result (with warnings?)
concreteToAbstractTerm' env localEnv =
  \case
    PsiReference  name -> case Map.lookup name localEnv of
     Just ref -> Right $ AstLocalRef name ref
     Nothing  -> case lookupCtxCurrent name env of
       Just ref -> Right $ AstRef name ref
       Nothing  -> case builtinDefinition name of
        Just term -> Right term
        Nothing   -> Left $ UnresolvedReferenceError name
    PsiLambda var body ->
     let binder   = inventBinder var LambdaBinder
         newLocal = Map.insert var binder localEnv
     in AstBind binder <$> recurEnv newLocal body
    PsiApplication f a -> $(each [| AstApp (~! recur f) (~! recur a) |])
    PsiLiteral   loc t -> Right $ AstLiteral loc t
    PsiImpossible  loc -> __TODO__
    PsiInaccessible  t -> __TODO__
    PsiMetaVar    name -> Right $ AstMetaVar name
    PsiTelescope var vis ty val -> do
      type' <- recur ty
      let binder   = AstBinderInfo
            { binderName = var
            , binderType = type'
            , binderKind = TelescopeBinder vis
            }
          newLocal = Map.insert var binder localEnv
      AstBind binder <$> recurEnv newLocal val
  where
    recur = concreteToAbstractTerm' env localEnv
    recurEnv = concreteToAbstractTerm' env
