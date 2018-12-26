{-# LANGUAGE DeriveGeneric #-}

module OwO.Syntax.TokenType
 ( LayoutContext(..)

 , TokenType(..)
 , PsiToken(..)
 , isStartingNewLayout
 -- Tokens

 , Name(..)
 , locationOfName
 , textOfName
 , hideName
 -- Names

 , AlexUserState(..)
 , alexInitUserState
 -- Alex
 ) where

import           Data.Text            as T

import           OwO.Syntax.Position
import qualified OwO.Util.StrictMaybe as Strict

import           GHC.Generics         (Generic)

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
  | NoName Loc        -- ^ @_@.
  deriving (Ord, Show)

locationOfName :: Name -> Loc
locationOfName (Name   l _) = l
locationOfName (NoName l)   = l

textOfName :: Name -> T.Text
textOfName (Name   _ n) = n
textOfName (NoName _)   = T.pack "_"

hideName :: Name -> Name
hideName (Name loc _) = NoName loc
hideName a = a

instance Eq Name where
  Name _ a == Name _ b = a == b
  _ == _ = False

data TokenType
  = ModuleToken
  -- ^ module
  | OpenToken
  -- ^ open
  | ImportToken
  -- ^ import
  | DataToken
  -- ^ data
  | CodataToken
  -- ^ codata
  | CaseToken
  -- ^ case (with abstraction)
  | CocaseToken
  -- ^ cocase (record constructor, coinductive)

  | InfixToken
  | InfixLToken
  | InfixRToken

  | WhereToken
  -- ^ where, starting a new layout
  | InstanceToken
  -- ^ instance, starting a new layout
  | PostulateToken
  -- ^ postulate, starting a new layout
  | OfToken
  -- ^ of (case), starting a new layout
  | DoToken
  -- ^ do, starting a new layout

  | BracketLToken
  -- ^ ], for `List` literal
  | BracketRToken
  -- ^ [, for `List` literal
  | ParenthesisLToken
  -- ^ (
  | ParenthesisRToken
  -- ^ )
  | SeparatorToken
  -- ^ |, with abstraction
  | IdiomBracketLToken
  -- ^ (|, idiom bracket
  | IdiomBracketRToken
  -- ^ |), idiom bracket
  | InstanceArgumentLToken
  -- ^ {|, instance argument
  | InstanceArgumentRToken
  -- ^ |}, instance argument
  | InaccessiblePatternLToken
  -- ^ [|, dot pattern
  | InaccessiblePatternRToken
  -- ^ |], dot pattern
  | ColonToken
  -- ^ :
  | LeftArrowToken
  -- ^ <-
  | RightArrowToken
  -- ^ ->
  | BraceLToken
  -- ^ {, implicit arguments, starts a layout
  | BraceRToken
  -- ^ }, finishes a layout
  | SemicolonToken
  -- ^ ;, finishes a line
  | EqualToken
  -- ^ =
  | DotToken
  -- ^ ., proof irrelevance, operators

  | IdentifierToken Name
  -- ^ identifier
  | OperatorToken Name
  -- ^ binary operators
  | StringToken T.Text
  -- ^ string literal
  | CharToken Char
  -- ^ character literal
  | IntegerToken Integer
  -- ^ integer numbers
  | CommentToken T.Text
  -- ^ comments. We reserve them to provide better tooling.
  -- ^ this shouldn't present in AST, only in token sequence.

  | EndOfFileToken
  -- ^ finishes a file. This will not present in the output token sequence.
  deriving (Eq, Generic, Ord, Show)

isStartingNewLayout :: TokenType -> Bool
isStartingNewLayout WhereToken     = True
isStartingNewLayout PostulateToken = True
isStartingNewLayout InstanceToken  = True
isStartingNewLayout DoToken        = True
isStartingNewLayout OfToken        = True
isStartingNewLayout CocaseToken    = True
isStartingNewLayout _              = False

data PsiToken = PsiToken
  { tokenType :: TokenType
  , location  :: Loc
  } deriving (Eq, Generic, Ord, Show)

data LayoutContext
  = NoLayout
  | Layout !Int
  deriving (Eq, Generic, Ord, Show)

-- | See @OwO.Syntax.Position@
data AlexUserState = AlexUserState
  { layoutStack    :: [LayoutContext]
  , currentFile    :: SrcFile
  , alexStartCodes :: [Int]
  } deriving (Eq, Generic, Show)

-- | See @OwO.Syntax.Position@
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { layoutStack    = []
  , currentFile    = Strict.Nothing
  , alexStartCodes = []
  }
