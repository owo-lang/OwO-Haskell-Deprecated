{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TypeOperators         #-}

-- | Concrete syntax tree
--   Prefixed with "Psi", stands for "Program Structure Item"
module OwO.Syntax.Concrete
  ( Visibility(..)

  , PsiTerm'(..)
  , PsiTerm
  , locationOfTerm
  , LiteralInfo(..)

  , PsiDataCons'(..)
  , PsiDataCons

  , PsiDataInfo'(..)
  , PsiDataInfo

  , PsiImplInfo'(..)
  , PsiImplInfo
  , nameOfImpl

  , FnPragma(..)
  , FnPragmas
  , DataPragma(..)
  , DataPragmas

  , PsiFile(..)
  , PsiFileType(..)
  , extensionOf
  , decideFileType
  , fileTypeAndExtensions

  , PsiDeclaration'(..)
  , PsiDeclaration

  , PsiFixityInfo'(..)
  , PsiFixityInfo
  , fixityInfo

  , QModuleName(..)
  , parentModule
  , hasParentModule
  ) where

import           Control.Applicative  ((<|>))
import           Data.Functor         ((<&>))
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.Text            as T
import           System.FilePath      (isExtensionOf)

import           OwO.Syntax.Common
import           OwO.Syntax.Module
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType (Name (..), locationOfName)

#include <impossible.h>

data Visibility
  = Explicit
  | Implicit
  | Instance
  deriving (Eq, Ord)

instance Show Visibility where
  show Explicit = "( )"
  show Implicit = "{ }"
  show Instance = "{| |}"

-- | The type parameter c is the name representation, which is probably Name
--   this is the AST for expressions (rhs) and pattern matching (lhs)
data PsiTerm' c
  = PsiReference c
  -- ^ A reference to a variable
  | PsiLambda c (PsiTerm' c)
  -- ^ Lambda abstraction, which has a name and a body
  | PsiApplication (PsiTerm' c) (PsiTerm' c)
  -- ^ Function application
  | PsiLiteral Loc LiteralInfo
  -- ^ constant
  | PsiImpossible Loc
  -- ^ Absurd pattern, impossible pattern
  | PsiInaccessible (PsiTerm' c)
  -- ^ Dotted pattern, inaccessible pattern
  | PsiMetaVar c
  -- ^ Meta variable
  | PsiTelescope c Visibility (PsiTerm' c) (PsiTerm' c)
  -- ^ Pi type, since "Pi" is too short, I pick the name "Telescope"
  -- binding name, visibility, type, and the body
  deriving (Eq, Ord, Show)

type PsiTerm = PsiTerm' Name

locationOfTerm :: PsiTerm -> Loc
locationOfTerm = \case
  PsiReference     n -> locationOfName n
  PsiLambda      n t -> locationOfName n `mergeLocations` locationOfTerm t
  PsiApplication f a -> locationOfTerm f `mergeLocations` locationOfTerm a
  PsiLiteral   loc _ -> loc
  PsiImpossible  loc -> loc
  PsiInaccessible  t -> locationOfTerm t
  PsiMetaVar       n -> locationOfName n
  PsiTelescope n _ _ r -> locationOfName n `mergeLocations` locationOfTerm r

-- | Program Structure Item: File Type
data PsiFileType
  = CodeFileType
  | LiterateFileType
  deriving (Eq, Ord)

instance Show PsiFileType where
  show CodeFileType     = "owo"
  show LiterateFileType = "lowo"

extensionOf :: PsiFileType -> FilePath
extensionOf = ('.' :) . show

decideFileType :: FilePath -> Maybe PsiFileType
decideFileType path = foldr f Nothing fileTypeAndExtensions
  where f (ext, ft) m = m <|> if ext `isExtensionOf` path
                              then Just ft else Nothing

fileTypeAndExtensions :: [(String, PsiFileType)]
fileTypeAndExtensions =
  [ CodeFileType
  , LiterateFileType
  ] <&> \ft -> (extensionOf ft, ft)

-- | Program Structure Item: File
data PsiFile = PsiFile
  { fileType           :: PsiFileType
  , topLevelModuleName :: QModuleName
  , declarations       :: [PsiDeclaration]
  , exposedFixityInfo  :: [PsiFixityInfo]
  } deriving (Eq, Ord, Show)

data PsiFixityInfo' c
  = PsiInfix  Int [c]
  | PsiInfixL Int [c]
  | PsiInfixR Int [c]
  deriving (Eq, Ord, Show)

fixityInfo :: PsiFixityInfo' c -> (String, Int, [c])
fixityInfo (PsiInfix  a b) = ("infix",  a, b)
fixityInfo (PsiInfixL a b) = ("infixl", a, b)
fixityInfo (PsiInfixR a b) = ("infixr", a, b)

-- | Function level pragma
data FnPragma
  = PgmNonTerminate
  -- ^ Do not reduce, disable termination check
  | PgmInstance
  -- ^ Add to instance search
  | PgmFailing
  -- ^ Supposed to raise an error (Maybe specify the error type?)
  | PgmTerminating
  -- ^ Disable termination check, unsafe
  deriving (Eq, Ord, Show)

type FnPragmas = [FnPragma]

data PsiDataCons' t c = PsiDataCons
  { dataConsName :: c
  , dataConsLoc  :: Loc
  , dataConsBody :: t c
  } deriving (Eq, Ord, Show)

-- | Inductive data family
data PsiDataInfo' t c
  = PsiDataDefinition
    { dataName     :: c
    , dataNameLoc  :: Loc
    , dataTypeCons :: t c
    , dataCons     :: [PsiDataCons' t c]
    }
  -- ^ An in-place definition
  | PsiDataSignature
    { dataName     :: c
    , dataNameLoc  :: Loc
    , dataTypeCons :: t c
    } deriving (Eq, Ord, Show)
  -- ^ A data type signature, for mutual recursion

-- | One clause of a top-level definition. Term arguments to constructors are:
--
-- 0. The function name
-- 1. The pattern matching expression, parsed as an application (missing for
--    PClauseR and PWithR because they're within a "with" clause)
-- 2. The list of extra 'with' patterns
-- 3. The right-hand side
-- 4. The where block (PsiDeclaration' t)
data PsiImplInfo' t c
  = PsiImplSimple c (t c) [t c] (t c) [PsiDeclaration' t c]
  -- ^ Most simple pattern
  -- TODO
  {-
  | PsiImplWith   c [t c] [t c] (t c) [PsiDeclaration' t c]
  -- ^ Pattern with 'with', we may use the keyword 'case'
  | PsiImplSimpleR        [t c] (t c) [PsiDeclaration' t c]
  -- ^ Most simple pattern
  | PsiImplWithR          [t c] (t c) [PsiDeclaration' t c]
  -- ^ Impl with 'with', we may use the keyword 'case'
  -}
  deriving (Eq, Ord, Show)

nameOfImpl :: PsiImplInfo -> Name
nameOfImpl (PsiImplSimple n _ _ _ _) = n
-- Impossible for PsiImplSimpleR, PsiImplWithR

data DataPragma
  = PgmNoPositivityCheck
  deriving (Eq, Ord, Show)

type DataPragmas = [DataPragma]

-- | Top-level declarations
--   TODOs: PsiCodata, PsiCopattern
data PsiDeclaration' t c
  = PsiTypeSignature c FnPragmas (t c)
  -- ^ Type signature
  | PsiSubmodule QModuleName [PsiFixityInfo' c] [PsiDeclaration' t c]
  -- ^ Module defined in modules
  | PsiPostulate c FnPragmas (t c)
  -- ^ Postulate, unsafe
{-
  | PsiPrimitive c (t c)
  -- ^ Primitive
-}
  | PsiData c DataPragmas (PsiDataInfo' t c)
  -- ^ Inductive data families
  | PsiImplementation c FnPragmas (NonEmpty (PsiImplInfo' t c))
  -- ^ A pattern matching clause which is an implementation of a function
  deriving (Eq, Ord, Show)

type PsiDeclaration = PsiDeclaration' PsiTerm' Name
type PsiDataCons    = PsiDataCons'    PsiTerm' Name
type PsiDataInfo    = PsiDataInfo'    PsiTerm' Name
type PsiImplInfo    = PsiImplInfo'    PsiTerm' Name
type PsiFixityInfo  = PsiFixityInfo'  Name
