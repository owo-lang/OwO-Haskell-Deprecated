module OwO.Options
  ( CompilerOptions(..)
  , PragmaOptions(..)
  ) where

import           Data.Function
import           Data.List

data CompilerOptions = CompilerOptions
  { optInputFile     :: FilePath
  , optIncludePaths  :: [FilePath]
  , optPragmaOptions :: PragmaOptions
  } deriving Show

data PragmaOptions = PragmaOptions
  { optNoPositivityCheck     :: Bool
  , optNoTerminationCheck    :: Bool
  , optNoExhaustivenessCheck :: Bool
  } deriving (Eq, Show)
