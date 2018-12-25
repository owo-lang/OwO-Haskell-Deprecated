{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

-- | Abstract syntax tree (see @OwO.Syntax.Concrete@)
--   Prefixed with "Ast", stands for "Abstract Syntax Tree"
module OwO.Syntax.Abstract where

import           Control.Applicative
import           Control.Monad
import           OwO.Syntax.Common
import qualified OwO.Syntax.Concrete  as C
import           OwO.Syntax.Position
import           OwO.Util.StrictMaybe as Strict

import           GHC.Generics         (Generic)

-- | All parameters becomes explicit in Ast
--   this stores the explicit/implicit information of Cst
data Visibility
  = Explicit
  | Implicit
  | Instance
  deriving (Eq, Ord)

instance Show Visibility where
  show Explicit = "(   )"
  show Implicit = "{   }"
  show Instance = "{| |}"

-- | @a@ is @C.Name@ or something
data AstTerm' c
  = AstConst Loc ConstInfo
  | AstRef c (AstDeclaration' AstTerm' c)
  | AstMetaVar Loc
  deriving (Eq, Ord, Show)

-- | Type constructors, data constructors
--   construct expressions under normal form
data AstConsInfo' t c = AstConsInfo
  { consName      :: c
  , consLoc       :: Loc
  , consSignature :: t c
  } deriving (Eq, Ord, Show)

data AstImplInfo' t c = AstImplInfo
  { implName :: c
  , implLoc  :: Loc
  , implType :: t c
  , implBody :: Strict.Maybe (t c)
  } deriving (Eq, Ord, Show)

-- | @a@ is @C.Name@ or something
data AstDeclaration' t c
  = AstTypeCons c (t c) [AstConsInfo' t c]
  -- ^ Type Constructors must be declared with a list of Data Constructors
  | AstDataCons (AstConsInfo' t c)
  -- ^ Data Constructors must be declared within a Type Constructor
  | AstImplementation (AstImplInfo' t c)
  -- ^ Function implementation, type, implementation
  deriving (Eq, Ord, Show)

type AstDeclaration = AstDeclaration' AstTerm' C.Name
type AstTerm        = AstTerm' C.Name
type AstConsInfo    = AstConsInfo' AstTerm' C.Name
type AstImplInfo    = AstImplInfo' AstTerm' C.Name
