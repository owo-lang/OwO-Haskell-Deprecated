{-# LANGUAGE CPP #-}

module OwO.Syntax.Position
  ( SrcFile

  -- Positions
  , Position'(..)
  , Position
  , PositionNoFile
  , positionInvariant
  , importantPart
  , simplePosition
  , emptyPosition
  , emptyPositionIn
  , emptyPositionInStr
  , positionWithFile

  -- Locations
  , Loc'(..)
  , Loc
  , LocNoFile
  , intervalInvariant
  , emptyLocation
  , emptyLocationIn
  , emptyLocationInStr
  , locationFromSegment
  , mergeLocations
  ) where

import           Data.Function
import           Data.Int
import           Data.Text            (Text)
import qualified Data.Text            as T

import           OwO.Util.List
import qualified OwO.Util.StrictMaybe as Strict

-- | Represents a point in the input.
--
--   If two positions have the same 'srcFile' and 'posPos' components,
--   then the final two components should be the same as well, but since
--   this can be hard to enforce the program should not rely too much on
--   the last two components; they are mainly there to improve error
--   messages for the user.
--
--   Note the invariant which positions have to satisfy: 'positionInvariant'.
data Position' a = Position
  { srcFile :: !a   -- ^ File.
  , posPos  :: !Int -- ^ Absolute position, counting from 0.
  , posLine :: !Int -- ^ Line number, counting from 1.
  , posCol  :: !Int -- ^ Column number, counting from 1.
  }

positionInvariant :: Position' a -> Bool
positionInvariant p = posPos p > 0 && posLine p > 0 && posCol p > 0

importantPart :: Position' a -> (a, Int)
importantPart p = (srcFile p, posPos p)

emptyPositionInStr :: String -> Position
emptyPositionInStr = emptyPositionIn . Strict.Just . T.pack

emptyPositionIn :: SrcFile -> Position
emptyPositionIn src = Position
  { srcFile = src
  , posPos  = 0
  , posLine = 0
  , posCol  = 0
  }

simplePosition :: Int -> Int -> Int -> PositionNoFile
simplePosition pos line col = Position
  { srcFile = ()
  , posPos  = pos
  , posLine = line
  , posCol  = col
  }

emptyPosition :: PositionNoFile
emptyPosition = simplePosition 0 0 0

positionWithFile :: PositionNoFile -> SrcFile -> Position
positionWithFile pos src = Position
  { srcFile = src
  , posPos  = posPos  pos
  , posCol  = posCol  pos
  , posLine = posLine pos
  }

instance Eq a => Eq (Position' a) where
  (==) = (==) `on` importantPart

instance Ord a => Ord (Position' a) where
  compare = compare `on` importantPart

instance Show a => Show (Position' a) where
  show (Position src pos line col) = "("
    ++ show pos
    ++ " "
    ++ show line
    ++ " "
    ++ show col
    ++ " in "
    ++ show src
    ++ ")"

-- | Absolute path
type SrcFile = Strict.Maybe Text

type Position       = Position' SrcFile
type PositionNoFile = Position' ()

-- | An interval. The @iEnd@ position is not included in the interval.
--
--   Note the invariant which intervals have to satisfy: 'intervalInvariant'.
data Loc' a = Loc
  { iStart
  , iEnd :: !(Position' a)
  } deriving (Eq, Ord)

instance Show a => Show (Loc' a) where
  show (Loc start end) = "(" ++ show start ++ ", " ++ show end ++ ")"

type Loc       = Loc' SrcFile
type LocNoFile = Loc' ()

intervalInvariant :: Ord a => Loc' a -> Bool
intervalInvariant i =
  all positionInvariant [iStart i, iEnd i] &&
  iStart i <= iEnd i &&
  srcFile (iStart i) == srcFile (iEnd i)

-- | Are the intervals consecutive and separated, do they all point to
--   the same file, and do they satisfy the interval invariant?
consecutiveAndSeparated :: Ord a => [Loc' a] -> Bool
consecutiveAndSeparated is =
  all intervalInvariant is &&
  allEqual (srcFile . iStart <$> is) &&
  (null is ||
   and (zipWith (<) (iEnd   <$> init is)
                    (iStart <$> tail is)))

mergeLocations :: Loc' a -> Loc' a -> Loc' a
mergeLocations (Loc start _) (Loc _ end) = Loc
  { iStart = start
  , iEnd   = end
  }

emptyLocationInStr :: String -> Loc
emptyLocationInStr = emptyLocationIn . Strict.Just . T.pack

emptyLocationIn :: SrcFile -> Loc
emptyLocationIn src = Loc
  { iStart = emptyPositionIn src
  , iEnd   = emptyPositionIn src
  }

emptyLocation :: LocNoFile
emptyLocation = Loc
  { iStart = emptyPosition
  , iEnd   = emptyPosition
  }

locationFromSegment :: PositionNoFile -> PositionNoFile -> SrcFile -> Loc
locationFromSegment start end src = Loc
  { iStart = positionWithFile start src
  , iEnd   = positionWithFile end   src
  }
