{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

-- | Concrete syntax tree
--   Prefixed with "Psi", stands for "Program Structure Item"
module OwO.Syntax.Concrete
  ( Name(..)
  , locationOfName
  , textOfName

  , PsiTerm'(..)
  , PsiTerm
  , ConstInfo(..)

  , PsiDataCons'(..)
  , PsiDataCons

  , PsiDataInfo'(..)
  , PsiDataInfo

  , PsiImplInfo'(..)
  , PsiImplInfo
  , functionNameOfImplementation

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

import           Control.Applicative ((<|>))
import           Data.Functor        ((<&>))
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.Text           as T
import           System.FilePath     (isExtensionOf)

import           OwO.Syntax.Common
import           OwO.Syntax.Module
import           OwO.Syntax.Position

#include <impossible.h>

-- | A name is a non-empty list of alternating 'Id's and 'Hole's. A normal name
--   is represented by a singleton list, and operators are represented by a list
--   with 'Hole's where the arguments should go. For instance:
--   @[Hole,Id "+",Hole]@
--   is infix addition.
--
--   Equality and ordering on @Name@s are defined to ignore interval so same
--   names in different locations are equal.
data Name
  = Name   Loc T.Text -- ^ A identifier.
  | NoName Loc NameId -- ^ @_@.
  deriving (Ord, Show)

locationOfName :: Name -> Loc
locationOfName (Name   l _) = l
locationOfName (NoName l _) = l

textOfName :: Name -> T.Text
textOfName (Name   _ n) = n
textOfName (NoName _ n) = T.pack $ '_' : show n

instance Eq Name where
  Name _ a == Name _ b = a == b
  _ == _ = False

-- | The type parameter c is the name representation, which is probably Name
--   this is the AST for expressions (rhs) and pattern matching (lhs)
data PsiTerm' c
  = PsiReference c
  -- ^ A reference to a variable
  | PsiLambda c (PsiTerm' c)
  -- ^ Lambda abstraction, which has a name and a body
  | PsiApplication (PsiTerm' c) (PsiTerm' c)
  -- ^ Function application
  | PsiConstant Loc ConstInfo
  -- ^ constant
  | PsiImpossible Loc
  -- ^ Absurd pattern, impossible pattern
  | PsiInaccessiblePattern (PsiTerm' c)
  -- ^ Dotted pattern, inaccessible pattern
  | PsiMetaVar c
  -- ^ Meta variable
  deriving (Eq, Ord, Show)

type PsiTerm = PsiTerm' Name

-- | Program Structure Item: File Type
data PsiFileType
  = CodeFileType
  | LiterateFileType
  deriving (Eq, Ord, Show)

extensionOf :: PsiFileType -> FilePath
extensionOf CodeFileType     = ".owo"
extensionOf LiterateFileType = ".lowo"

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
fixityInfo (PsiInfix  a b) = ("infix", a, b)
fixityInfo (PsiInfixL a b) = ("infixl", a, b)
fixityInfo (PsiInfixR a b) = ("infixr", a, b)

-- | Function level pragma
data FnPragma
  = NonTerminate
  -- ^ Do not reduce, disable termination check
  | Instance
  -- ^ Add to instance search
  | Failing
  -- ^ Supposed to raise an error (Maybe specify the error type?)
  | Terminating
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

functionNameOfImplementation :: PsiImplInfo -> Name
functionNameOfImplementation (PsiImplSimple n _ _ _ _) = n
-- Impossible for PsiImplSimpleR, PsiImplWithR

data DataPragma
  = NoPositivityCheck
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
  | PsiData c DataPragmas (PsiDataInfo' t c)
  -- ^ Inductive data families
-}
  | PsiImplementation c FnPragmas (NonEmpty (PsiImplInfo' t c))
  -- ^ A pattern matching clause which is an implementation of a function
  deriving (Eq, Ord, Show)

type PsiDeclaration = PsiDeclaration' PsiTerm' Name
type PsiDataCons    = PsiDataCons'    PsiTerm' Name
type PsiDataInfo    = PsiDataInfo'    PsiTerm' Name
type PsiImplInfo    = PsiImplInfo'    PsiTerm' Name
type PsiFixityInfo  = PsiFixityInfo'  Name
