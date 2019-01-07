module OwO.Util.Applicative where

import           Control.Applicative

ifM :: Applicative m => Bool -> m () -> m ()
ifM True  m = m
ifM False _ = pure ()

unlessM :: Applicative m => Bool -> m () -> m ()
unlessM = ifM . not

pure2 :: (Applicative m1, Applicative m2) => a -> m1 (m2 a)
pure2 = pure . pure
