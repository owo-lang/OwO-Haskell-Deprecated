{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

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

import           Control.Monad.Except     (MonadError (..))
import           Control.Monad.State      (MonadState (..), get, modify, put)
import           Data.Functor             ((<&>))
import           Data.List                (findIndex, partition)
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
import           OwO.Util.Applicative
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
  deriving (Eq, Show)

type DesugarMonad m = (MonadState AstContext m, MonadError DesugarError m)

concreteToAbstractDecl
  :: DesugarMonad m
  => [PsiDeclaration]
  -> m [DesugarError]
concreteToAbstractDecl = concreteToAbstractDecl' []

concreteToAbstractTerm
  :: DesugarMonad m
  => PsiTerm
  -> m AstTerm
concreteToAbstractTerm = concreteToAbstractTerm' []

concreteToAbstractDecl'
  :: DesugarMonad m
  => [TypeSignature]
  -- Unimplemented declarations
  -> [PsiDeclaration]
  -- Unchecked declarations
  -> m [DesugarError]
  -- Errors, or type-checking result with warnings
concreteToAbstractDecl' [] [] = return []
concreteToAbstractDecl' sb [] = do
    env <- get
    put $ foldr (uncurry addDefinition) env newDecls
    pure $ NoImplementationError <$> sb
  where
    newDecls = sb <&> \(name, _, term) -> (name, AstPostulate name term)
concreteToAbstractDecl' sigs (d : ds) =
  case d of
    PsiTypeSignature name pgms sig -> do
      term <- checkTerm sig
      checkRest $ (name, pgms, term) : sigs
    PsiImplementation name pgms clauses ->
      let
        -- TODO deal with pragmas
        -- Type signature, clauses
        desugarFunction (_, tyPgms, ty) = case clauses of
          ((PsiImplSimple name pattern [] rhs whereBlock) :| []) -> do
            body <- checkTerm rhs
            modify . addDefinition name $
              AstImplementation AstImplInfo
                { implName = name
                , implType = ty
                , implBody = body
                }
          _ -> __TODO__
      in case partition ((== name) . fst3) sigs of
        (a : b : _,_) -> throwError $ DuplicatedTypeSignatureError (a, b)
        (sigs,  rest) -> do
          desugarFunction $ case sigs of
            [a] -> a
            [ ] -> (name, [], inventMetaVar name)
            _   -> __IMPOSSIBLE__
          checkRest rest
    -- TODO deal with pragmas
    PsiPostulate name pgms ty -> do
      ty' <- checkTerm ty
      modify $ addDefinition name (AstPostulate name ty')
      checkRest sigs
    _ -> return __TODO__
  where
    checkRest sig = concreteToAbstractDecl' sig ds
    checkTerm = concreteToAbstractTerm' []

concreteToAbstractTerm'
  :: DesugarMonad m
  => [AstBinderInfo]
  -- Local variables
  -> PsiTerm
  -- Input term
  -> m AstTerm
  -- Errors, or type-checking result (with warnings?)
concreteToAbstractTerm' localEnv =
  \case
    PsiReference  name -> case findIndex ((name ==) . binderName) localEnv of
      Just index -> pure $ AstLocalRef name index
      Nothing  -> do
        env <- get
        case lookupCtxCurrent name env of
          Just ref -> pure $ AstRef name ref
          Nothing  -> case builtinDefinition name of
            Just term -> pure term
            Nothing   -> throwError $ UnresolvedReferenceError name
    PsiLambda var body ->
     let binder   = inventBinder var LambdaBinder
     in AstBind binder <$> recurEnv (binder : localEnv) body
    PsiApplication f a -> $(each [| AstApp (~! recur f) (~! recur a) |])
    PsiLiteral   loc t -> pure $ AstLiteral loc t
    PsiImpossible  loc -> __TODO__
    PsiInaccessible  t -> __TODO__
    PsiMetaVar    name -> pure $ AstMetaVar name
    PsiTelescope var vis ty val -> do
      type' <- recur ty
      let binder   = AstBinderInfo
            { binderName = var
            , binderType = type'
            , binderKind = TelescopeBinder vis
            }
      AstBind binder <$> recurEnv (binder : localEnv) val
  where
    recur = concreteToAbstractTerm' localEnv
    recurEnv = concreteToAbstractTerm'
