-- | An interface for reporting \"impossible\" errors
--   https://github.com/agda/agda/blob/master/src/full/Agda/Utils/Impossible.hs

module OwO.Util.Impossible
  ( Impossible(..)
  , throwImpossible
  , catchImpossible
  ) where

import           Control.Exception as E

-- | \"Impossible\" errors, annotated with a file name and a line
--   number corresponding to the source code location of the error.

data Impossible
  = Impossible String Integer
    -- ^ We reached a program point which should be unreachable.

  | Unreachable String Integer
    -- ^ @Impossible@ with a different error message.
    --   Used when we reach a program point which can in principle
    --   be reached, but not for a certain run.

  | Unimplemented String Integer
    -- ^ A TODO.

instance Show Impossible where
  -- Sell moe
  show (Impossible file line) = unlines
    [ "An internal ewor has occurred. Pwease rwepowt this as a bug OwO."
    , "Location of the erwor: " ++ file ++ ":" ++ show line
    ]
  show (Unreachable file line) = unlines
    [ "We rweached a pwogwam point we did not want to rweach OwO."
    , "Location of the erwor: " ++ file ++ ":" ++ show line
    ]
  show (Unimplemented file line) = unlines
    [ "We rweached a TODO, pwease wait until we impwement this OwO."
    , "Location of the TODO: " ++ file ++ ":" ++ show line
    ]

instance Exception Impossible

-- | Abort by throwing an \"impossible\" error. You should not use
-- this function directly. Instead use the macro in @undefined.h@.

throwImpossible :: Impossible -> a
throwImpossible = throw

-- | Catch an \"impossible\" error, if possible.

catchImpossible :: IO a -> (Impossible -> IO a) -> IO a
catchImpossible = E.catch
