module OwO.Syntax.Parser.Alex where

import           OwO.Syntax.Position
import qualified OwO.Util.StrictMaybe as Strict

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
