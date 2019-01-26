{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

-- | OwO's type checking state is a state monad transformer.
--   We call it TypeCheckingMonad, in short TCM, as Agda does.
module OwO.TypeChecking.Monad where

import           Control.Monad.Except     (MonadError (..))
import           Control.Monad.State      (MonadState (..))

import qualified Data.Text                as T

import           OwO.Options
import           OwO.Syntax.Concrete      (PsiTerm, PsiTerm' (..))
import           OwO.Syntax.Context       (Context (..), emptyCtx)
import           OwO.Syntax.Module
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType     (Name (..))
import           OwO.TypeChecking.Core
import           OwO.TypeChecking.Desugar (DesugarError (..))

#include <impossible.h>

-- | TypeChecking State. I haven't decide on whether to store warnings here
--   (but errors should definitely be in the other side of the Monad)
data TCState = TypeCheckingState
  { stateOptions     :: CompilerOptions
  -- ^ This is passed all around
  , stateDefinitions :: Context Definition
  -- ^ A symbol table containing all type-checked definitions
  } deriving Show

emptyTCState :: CompilerOptions -> TCState
emptyTCState opts = TypeCheckingState
  { stateOptions     = opts
  , stateDefinitions = emptyCtx
  }

-- | TypeChecking Environment
data TCEnv = TypeCheckingEnv
  { envState       :: TCState
  -- ^ This is passed all around
  , envDefinitions :: Context Definition
  -- ^ Local definitions
  , envModuleName  :: QModuleName
  -- ^ Current module name
  } deriving Show

-- | TypeChecking Error
data TCError
  = DesugarErr DesugarError
  | OtherErr String
  deriving (Eq, Show)

type TCM m = (MonadState TCState m, MonadError TCError m)
