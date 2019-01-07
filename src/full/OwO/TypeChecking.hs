{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module OwO.TypeChecking where

import           Control.Applicative      (Alternative (..))
import           Control.Monad.State      (runState)
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

typeCheckFile :: TCState -> PsiFile -> TCM ()
typeCheckFile state file = do
  let decls        = declarations file
    --   (ctx, warns) = runExceptT $ runStateT
    --     (concreteToAbstractDecl decls) emptyCtx
      moduleName   = topLevelModuleName file
  -- TODO:
  --  Invoke `Abstract.concreteToAbstract*`
  --  Then type check the implemented functions
  return __TODO__
