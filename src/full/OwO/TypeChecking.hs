{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module OwO.TypeChecking where

import           Control.Applicative     (Alternative (..))
import           Data.Functor            ((<&>))
import           Data.Maybe              (catMaybes)

import           OwO.Syntax.Concrete
    ( PsiFile (..)
    , PsiTerm
    , PsiTerm' (..)
    )
import           OwO.Syntax.Context
import           OwO.Syntax.TokenType    (textOfName)
import           OwO.TypeChecking.Core
import           OwO.TypeChecking.Match
import           OwO.TypeChecking.Monad
import           OwO.TypeChecking.Reduce

#include <impossible.h>

constType :: ConstInfo -> Type
constType = __TODO__

-- | TODO refactor after Ast/Cst distinction is done
typeCheck :: TCEnv -> PsiTerm -> Either TCErr (Type, Term)
typeCheck env (PsiConstant _ info) = Right (constType info, Const info)
typeCheck env (PsiLambda binder term) = return __TODO__
typeCheck env (PsiApplication func term) = do
  (f, ft) <- typeCheck env func
  (x, xt) <- typeCheck env term
  -- TODO: check the relationship
  return (__TODO__, App f x)
typeCheck env (PsiReference name) =
  case contextual <|> builtin of
    Nothing  -> Left  $ UnresolvedReferenceErr name
    Just def -> Right (definitionType def, Ref FunctionName name def)
  where
    builtin = builtinDefinition $ textOfName name
    contextual = lookupCtxCurrent name $ envDefinitions env
typeCheck env term = return __TODO__

typeCheckFile :: TCState -> PsiFile -> TCM ()
typeCheckFile state file = do
  let decls      = declarations file
  let moduleName = topLevelModuleName file
  -- TODO:
  --  Look for definitions, give warnings about unimplemented definitions
  --  Then type check the implemented ones
  return __TODO__
