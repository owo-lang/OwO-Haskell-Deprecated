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
