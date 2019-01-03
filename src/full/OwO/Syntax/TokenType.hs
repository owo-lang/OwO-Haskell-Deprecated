{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

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

-- | Identifier representation, with a text and the source location
data Name
  = Name   Loc T.Text -- ^ A identifier.
  | NoName Loc        -- ^ Anonymous identifier, like compiler-generated.
  deriving Show

locationOfName :: Name -> Loc
locationOfName (Name   l _) = l
locationOfName (NoName l)   = l

textOfName :: Name -> T.Text
textOfName (Name   _ n) = n
textOfName (NoName _)   = "_"

hideName :: Name -> Name
hideName (Name loc _) = NoName loc
hideName a = a

instance Eq Name where
  Name _ a == Name _ b = a == b
  _ == _ = False

instance Ord Name where
  Name _ a `compare` Name _ b = a `compare` b
  _ `compare` _ = EQ

data TokenType
  = ModuleToken
  -- ^ @module@
  | OpenToken
  -- ^ @open@
  | ImportToken
  -- ^ @import@
  | DataToken
  -- ^ @data@
  | CodataToken
  -- ^ @codata@
  | CaseToken
  -- ^ @case@ (with abstraction)
  | CocaseToken
  -- ^ @cocase@ (record constructor, coinductive)

  | InfixToken
  | InfixLToken
  | InfixRToken

  | WhereToken
  -- ^ @where@, starting a new layout
  | InstanceToken
  -- ^ @instance@, starting a new layout
  | PostulateToken
  -- ^ @postulate@, starting a new layout
  | OfToken
  -- ^ @of@ (@case@), starting a new layout
  | DoToken
  -- ^ @do@, starting a new layout

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
  | BackslashToken
  -- ^ \, for lambdas

  | IdentifierToken Name
  -- ^ Identifiers
  | MetaVarToken Name
  -- ^ _, for (named) meta vars and ignoring patterns
  | OperatorToken Name
  -- ^ Binary operators
  | StringToken T.Text
  -- ^ String literals
  | CharToken Char
  -- ^ Character literals
  | IntegerToken Integer
  -- ^ Integer number literals
  | CommentToken T.Text
  -- ^ Comments. We reserve them to provide better tooling.
  -- ^ This shouldn't present in AST, only in token sequence.

  | EndOfFileToken
  -- ^ Finishes a file. This will not present in the output token sequence.
  deriving (Eq, Ord, Show)

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
  } deriving (Eq, Ord, Show)

data LayoutContext
  = NoLayout
  | Layout !Int
  deriving (Eq, Ord, Show)

-- | See @OwO.Syntax.Position@
data AlexUserState = AlexUserState
  { layoutStack    :: [LayoutContext]
  , currentFile    :: SrcFile
  , alexStartCodes :: [Int]
  } deriving (Eq, Show)

-- | See @OwO.Syntax.Position@
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { layoutStack    = []
  , currentFile    = Strict.Nothing
  , alexStartCodes = []
  }
