module OwO.Syntax.Module
  ( QModuleName (..)
  , hasParentModule
  , parentModule
  ) where

import qualified Data.Text as T

-- | Qualified Name
newtype QModuleName = QModuleName { moduleNameList :: [T.Text] }
  deriving (Eq, Ord)

hasParentModule :: QModuleName -> Bool
hasParentModule (QModuleName ls) = ls /= []

parentModule :: QModuleName -> Maybe QModuleName
parentModule (QModuleName list) = case list of
  [      ] -> Nothing
  (l : ls) -> Just $ QModuleName { moduleNameList = ls }

instance Show QModuleName where
  show (QModuleName ls) = concat $ ('.' :) . T.unpack <$> ls

