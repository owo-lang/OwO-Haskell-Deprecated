{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections         #-}

-- | OwO's type checking state is a state monad transformer.
--   We call it TypeCheckingMonad, in short TCM, as Agda does.
module OwO.TypeChecking.Monad where

import           Control.Applicative        (Alternative (..), (<|>))
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map                   as Map
import qualified Data.Text                  as T

import           OwO.Options
import           OwO.Syntax.Common
import           OwO.Syntax.Concrete        (PsiTerm, PsiTerm' (..))
import           OwO.Syntax.Context         (Context (..), emptyCtx)
import           OwO.Syntax.Module
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType       (Name (..))
import           OwO.TypeChecking.Core

import           GHC.Generics               (Generic)

#include <impossible.h>

-- | TypeChecking State. I haven't decide on whether to store warnings here
--   (but errors should definitely be in the other side of the Monad)
data TCState = TypeCheckingState
  { stateOptions     :: CompilerOptions
  -- ^ This is passed all around
  , stateDefinitions :: Context Definition
  -- ^ A symbol table containing all type-checked definitions
  } deriving (Generic, Show)

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
  } deriving (Generic, Show)

data TCErr' t
  = OtherErr t
  | UnresolvedReferenceErr Name
  deriving (Eq, Functor, Show)

-- | TypeChecking Error
type TCErr = TCErr' PsiTerm

-- | TypeChecking Monad Transformer
type TCMT m = StateT TCState (ExceptT TCErr m)

-- | TypeChecking Monad
type TCM = TCMT IO
