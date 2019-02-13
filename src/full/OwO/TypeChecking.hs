{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module OwO.TypeChecking where

import           Control.Applicative      (Alternative (..))
import           Control.Monad.Except     (MonadError (..), runExceptT)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Control.Monad.State
    ( MonadState (..)
    , get
    , modify
    , put
    , runStateT
    )
import           Data.Functor             ((<&>))
import           Data.Maybe               (catMaybes)

import           OwO.Syntax.Abstract
import           OwO.Syntax.Concrete
    ( LiteralInfo
    , PsiFile (..)
    , PsiTerm
    , PsiTerm' (..)
    )
import           OwO.Syntax.Context
import           OwO.Syntax.TokenType     (textOfName)
import           OwO.TypeChecking.Core
import           OwO.TypeChecking.Desugar
import           OwO.TypeChecking.Match
import           OwO.TypeChecking.Monad
import           OwO.TypeChecking.Reduce

#include <impossible.h>

literalType :: LiteralInfo -> Type
literalType _ = Var __TODO__

-- | Check if some @AstTerm@ is a type-correct term of type @Type@
checkExpr :: TCM m => AstTerm -> Type -> m Term
checkExpr term ty = case term of
  AstTypeLit name level -> do
    typeLevel <- case ty of
      TType l -> pure l
      _       -> throwError $ TypeMismatch "Types' types should universes"
    termLevel <- case level of
      ULevelMax   -> pure ULevelMax
      ULevelVar v -> ULevelVar <$> checkInfer v
      ULevelLit i -> pure $ ULevelLit i
    case typeLevel of
      ULevelMax     -> pure $ TType termLevel
      ULevelVar var -> pure __TODO__
      ULevelLit _   -> if succ termLevel == typeLevel
        then pure $ TType termLevel
        else throwError $ TypeMismatch "Universe Level doesn't match"
  AstMetaVar _ -> throwError __TODO__
  AstBind binder body -> throwError __TODO__
  _ -> throwError __TODO__

-- | Convert an @AstTerm@ into a @Term@ without type restriction
checkInfer :: TCM m => AstTerm -> m Term
checkInfer = \case
  AstTypeLit name level -> case level of
    ULevelLit lit -> pure . TType $ ULevelLit lit
    ULevelVar var -> TType . ULevelVar <$> checkInfer var
    ULevelMax     -> pure $ TType ULevelMax
  AstLocalRef name index -> pure $ Var index
  AstMetaVar name -> pure $ Meta name
  AstBind binder body -> do
    let name = binderName binder
    ty <- checkInfer $ binderType binder
    kd <- case binderKind binder of
      LambdaBinder      -> pure Lambda
      -- TODO: assignment
      LetBinder       t -> pure $ Telescope ty
      -- TODO: assignment
      GeneratedBinder t -> pure $ Telescope ty
      TelescopeBinder _ -> throwError __TODO__
    -- TODO: add telescope item to context
    bd <- checkInfer body
    pure $ Bind name kd bd
  _ -> pure __TODO__

typeCheckFile :: TCM m => PsiFile -> m ()
typeCheckFile file = do
  let decls       = declarations file
      moduleName  = topLevelModuleName file
  desugarResult <- runExceptT . flip runStateT emptyCtx $
    concreteToAbstractDecl decls
  (ctx, warns) <- case desugarResult of
    Left err -> throwError $ DesugarErr err
    Right ok -> pure ok
  -- TODO:
  --  Type check the implemented functions
  return __TODO__
