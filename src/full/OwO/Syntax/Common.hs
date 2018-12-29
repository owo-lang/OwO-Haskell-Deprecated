{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module OwO.Syntax.Common where

import qualified Data.Text            as T
import           Data.Word

import qualified OwO.Util.StrictMaybe as Strict

import           GHC.Generics         (Generic)

#include <impossible.h>

--------------------------------------------------------------------------------
-- * Meta variables
--------------------------------------------------------------------------------

-- | A meta variable identifier is just a natural number.
--   It can have a name, as in Idris.
data MetaId = MetaId
  { metaId   :: !Word64
  , metaName :: Strict.Maybe String
  } deriving (Eq, Generic, Ord)

-- | Show non-record version of this newtype.
instance Show MetaId where
  showsPrec p (MetaId n m) = showParen (p > 0) $
    showString ("MetaId " ++ Strict.fromMaybe "null" m ++ " ") . shows n

data LiteralInfo
  = IntLit Int
  | IntegerLit Integer
  | StringLit T.Text
  | CharLit Char
  deriving (Eq, Generic, Ord, Show)
