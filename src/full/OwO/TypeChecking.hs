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
      _       -> throwError $ TypeMismatch "Type mismatch"
    termLevel <- case level of
      ULevelLit i -> pure $ ULevelLit i
      ULevelVar v -> ULevelVar <$> checkInfer v
      ULevelMax   -> pure ULevelMax
    case typeLevel of
      ULevelLit _ | succ termLevel == typeLevel -> pure $ TType termLevel
      ULevelVar var -> pure __TODO__
      ULevelMax     -> pure $ TType termLevel
  _ -> throwError __TODO__

-- | Infer the type of an @AstTerm@
checkInfer :: TCM m => AstTerm -> m Term
checkInfer = \case
  AstTypeLit name level -> case level of
    ULevelLit lit -> pure . TType $ ULevelLit lit
    ULevelVar var -> TType . ULevelVar <$> checkInfer var
    ULevelMax     -> pure $ TType ULevelMax
  AstLocalRef name index -> pure $ Var index
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
