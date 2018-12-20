{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module OwO.TypeChecking where

import           Data.Functor             ((<&>))
import           Data.Maybe               (catMaybes)

import           OwO.Syntax.Abstract
import           OwO.TypeChecking.Core
import           OwO.TypeChecking.Match
import           OwO.TypeChecking.Monad
import           OwO.TypeChecking.Reduce

#include <impossible.h>

typeCheck :: TCEnv -> PsiTerm -> Either TCErr Term
typeCheck env (PsiConstant _ info) = Right $ Constant info
typeCheck env (PsiLambda binder term) = return __TODO__
typeCheck env (PsiReference name) = return __TODO__
typeCheck env term = return __TODO__

typeCheckFile :: TCState -> PsiFile -> TCM ()
typeCheckFile state file = do
  let decls      = declarations file
  let moduleName = topLevelModuleName file
  -- TODO:
  --  Look for definitions, give warnings about unimplemented definitions
  --  Then type check the implemented ones
  return __TODO__
