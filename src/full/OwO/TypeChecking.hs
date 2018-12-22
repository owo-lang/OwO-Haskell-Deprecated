{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module OwO.TypeChecking where

import           Control.Applicative     (Alternative (..))
import           Data.Functor            ((<&>))
import           Data.Maybe              (catMaybes)
import           Each

import           OwO.Syntax.Abstract
import           OwO.TypeChecking.Core
import           OwO.TypeChecking.Match
import           OwO.TypeChecking.Monad
import           OwO.TypeChecking.Reduce

#include <impossible.h>

typeCheck :: TCEnv -> PsiTerm -> Either TCErr Term
typeCheck env (PsiConstant _ info) = Right $ Constant info
typeCheck env (PsiLambda binder term) = return __TODO__
typeCheck env (PsiApplication func term) = $(each [|
  App (~! typeCheck env func) (~! typeCheck env term) |] )
typeCheck env (PsiReference name) =
  case contextual <|> builtin of
    Nothing  -> Left  $ UnresolvedReferenceErr name
    Just def -> Right $ Ref FunctionName name def
  where
    txt = textOfName name
    builtin = builtinDefinition txt
    modName = envModuleName env
    contextual = lookupCtxWithName modName txt $ envDefinitions env
typeCheck env term = return __TODO__

typeCheckFile :: TCState -> PsiFile -> TCM ()
typeCheckFile state file = do
  let decls      = declarations file
  let moduleName = topLevelModuleName file
  -- TODO:
  --  Look for definitions, give warnings about unimplemented definitions
  --  Then type check the implemented ones
  return __TODO__
