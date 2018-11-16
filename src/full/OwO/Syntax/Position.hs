{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}

module OwO.Syntax.Position where

import Data.Foldable (Foldable)
import qualified Data.Foldable as Fold
import Data.Function
import Data.Int
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)

import qualified OwO.Util.StrictMaybe as Strict
import OwO.Util.List

-- | Represents a point in the input.
--
-- If two positions have the same 'srcFile' and 'posPos' components,
-- then the final two components should be the same as well, but since
-- this can be hard to enforce the program should not rely too much on
-- the last two components; they are mainly there to improve error
-- messages for the user.
--
-- Note the invariant which positions have to satisfy: 'positionInvariant'.
data Position' a = Pn
  { srcFile :: !a     -- ^ File.
  , posPos  :: !Int32 -- ^ Position, counting from 0.
  , posLine :: !Int32 -- ^ Line number, counting from 0.
  , posCol  :: !Int32 -- ^ Column number, counting from 0.
  } deriving (Show)

positionInvariant :: Position' a -> Bool
positionInvariant p = posPos p > 0 && posLine p > 0 && posCol p > 0

importantPart :: Position' a -> (a, Int32)
importantPart p = (srcFile p, posPos p)

instance Eq a => Eq (Position' a) where
  (==) = (==) `on` importantPart

instance Ord a => Ord (Position' a) where
  compare = compare `on` importantPart

-- | Absolute path
type SrcFile = Strict.Maybe Text

type Position       = Position' SrcFile
type PositionNoFile = Position' ()

-- | A range is a file name, plus a sequence of intervals, assumed to
-- point to the given file. The intervals should be consecutive and
-- separated.
--
-- Note the invariant which ranges have to satisfy: 'rangeInvariant'.
data Range' a
  = NoRange
  | Range !a (Seq IntervalNoFile)

deriving instance Show a => Show (Range' a)
type Range = Range' SrcFile

-- | An interval. The @iEnd@ position is not included in the interval.
--
-- Note the invariant which intervals have to satisfy: 'intervalInvariant'.
data Interval' a = Interval { iStart, iEnd :: !(Position' a) }

deriving instance Show a => Show (Interval' a)
type Interval       = Interval' SrcFile
type IntervalNoFile = Interval' ()

intervalInvariant :: Ord a => Interval' a -> Bool
intervalInvariant i =
  all positionInvariant [iStart i, iEnd i] &&
  iStart i <= iEnd i &&
  srcFile (iStart i) == srcFile (iEnd i)

-- | Are the intervals consecutive and separated, do they all point to
--   the same file, and do they satisfy the interval invariant?
consecutiveAndSeparated :: Ord a => [Interval' a] -> Bool
consecutiveAndSeparated is =
  all intervalInvariant is &&
  allEqual ((srcFile . iStart) <$> is) &&
  (null is ||
   and (zipWith (<) (iEnd   <$> init is)
                    (iStart <$> tail is)))

-- | Range invariant.
rangeInvariant :: Ord a => Range' a -> Bool
rangeInvariant r =
  consecutiveAndSeparated (rangeIntervals r) &&
  case r of
    Range _ is -> not (null is)
    NoRange    -> True

-- | The intervals that make up the range. The intervals are
-- consecutive and separated ('consecutiveAndSeparated').
rangeIntervals :: Range' a -> [IntervalNoFile]
rangeIntervals NoRange      = []
rangeIntervals (Range _ is) = Fold.toList is

-- | Turns a file name plus a list of intervals into a range.
--
-- Precondition: 'consecutiveAndSeparated'.
intervalsToRange :: a -> [IntervalNoFile] -> Range' a
intervalsToRange _ [] = NoRange
intervalsToRange f is = Range f (Seq.fromList is)
