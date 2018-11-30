{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module OwO.Syntax.Abstract
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

  , PsiPatternInfo'(..)
  , PsiPatternInfo

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
import qualified Data.Text           as T
import           Data.Functor        ((<&>))
import           System.FilePath     (isExtensionOf)

import           OwO.Syntax.Common
import           OwO.Syntax.Position

import           GHC.Generics        (Generic)

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
  deriving (Generic, Ord, Show)

locationOfName :: Name -> Loc
locationOfName (Name   l _) = l
locationOfName (NoName l _) = l

textOfName :: Name -> T.Text
textOfName (Name   _ n) = n
textOfName (NoName _ n) = T.pack $ '_' : show n

instance Eq Name where
  Name _ a == Name _ b = a == b
  _ == _ = False

data ConstInfo
  = IntConst Int
  | IntegerConst Integer
  | StringConst T.Text
  | CharConst Char
  deriving (Eq, Generic, Ord, Show)

-- | The type parameter c is the name representation, which is probably C.Name
data PsiTerm' c
  = PsiReference c
  -- ^ A reference to a variable
  {-
  | PsiLambda Loc c (PsiTerm' c) (PsiTerm' c)
  -}
  | PsiPatternVar c
  -- ^ Pattern variable
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
  deriving (Eq, Generic, Ord, Show)

type PsiTerm = PsiTerm' Name

-- | Program Structure Item: File Type
data PsiFileType
  = CodeFileType
  | LiterateFileType
  deriving (Eq, Generic, Ord, Show)

extensionOf :: PsiFileType -> String
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

-- | Qualified Name
newtype QModuleName = QModuleName { moduleNameList :: [T.Text] }
  deriving (Eq, Generic, Ord)

hasParentModule :: QModuleName -> Bool
hasParentModule (QModuleName ls) = ls /= []

parentModule :: QModuleName -> Maybe QModuleName
parentModule (QModuleName list) = case list of
  [      ] -> Nothing
  (l : ls) -> Just $ QModuleName { moduleNameList = ls }

instance Show QModuleName where
  show (QModuleName ls) = concat $ ('.' :) . T.unpack <$> ls

-- | Program Structure Item: File
data PsiFile = PsiFile
  { fileType           :: PsiFileType
  , topLevelModuleName :: QModuleName
  , declarations       :: [PsiDeclaration]
  } deriving (Eq, Generic, Ord, Show)

data PsiFixityInfo' c
  = PsiInfix  Int [c]
  | PsiInfixL Int [c]
  | PsiInfixR Int [c]
  deriving (Eq, Functor, Generic, Ord, Show)

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
  deriving (Eq, Generic, Ord, Show)

type FnPragmas = [FnPragma]

data PsiDataCons' t c = PsiDataCons
  { dataConsName :: c
  , dataConsLoc  :: Loc
  , dataConsBody :: t c
  } deriving (Eq, Functor, Generic, Ord, Show)

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
    } deriving (Eq, Functor, Generic, Ord, Show)
  -- ^ A data type signature, for mutual recursion

-- | One clause of a top-level definition. Term arguments to constructors are:
--
-- 1. The whole application (missing for PClauseR and PWithR because they're
--    within a "with" clause)
-- 2. The list of extra 'with' patterns
-- 3. The right-hand side
-- 4. The where block (PsiDeclaration' t)
data PsiPatternInfo' t c
  = PsiPatternSimple  Loc c (t c) [t c] (t c) [PsiDeclaration' t c]
  -- ^ Most simple pattern
  -- TODO
  {-
  | PsiPatternWith    Loc c (t c) [t c] (t c) [PsiDeclaration' t c]
  -- ^ Pattern with 'with', we may use the keyword 'case'
  | PsiPatternSimpleR Loc         [t c] (t c) [PsiDeclaration' t c]
  -- ^ Most simple pattern
  | PsiPatternWithR   Loc         [t c] (t c) [PsiDeclaration' t c]
  -- ^ Pattern with 'with', we may use the keyword 'case'
  -}
  deriving (Eq, Generic, Functor, Ord, Show)

data DataPragma
  = NoPositivityCheck
  deriving (Eq, Generic, Ord, Show)

type DataPragmas = [DataPragma]

-- | Top-level declarations
--   TODOs: PsiCodata, PsiPattern, PsiCopattern
data PsiDeclaration' t c
  = PsiFixity (PsiFixityInfo' c)
  -- ^ (infix, infixl, infixr), priority, symbols
  | PsiTypeSignature c FnPragmas (t c)
  -- ^ Type signature
  | PsiSubmodule QModuleName [PsiDeclaration' t c]
  -- ^ Module defined in modules
  | PsiPostulate c FnPragmas (t c)
  -- ^ Postulate, unsafe
  | PsiPrimitive c (t c)
  -- ^ Primitive
  | PsiData c DataPragmas (PsiDataInfo' t c)
  -- ^ Inductive data families
  | PsiPattern c FnPragmas [PsiPatternInfo' t c] (t c)
  -- ^ A pattern matching clause
  deriving (Eq, Functor, Generic, Ord, Show)

type PsiDeclaration = PsiDeclaration' PsiTerm' Name
type PsiDataCons    = PsiDataCons'    PsiTerm' Name
type PsiDataInfo    = PsiDataInfo'    PsiTerm' Name
type PsiPatternInfo = PsiPatternInfo' PsiTerm' Name
type PsiFixityInfo  = PsiFixityInfo'  Name
