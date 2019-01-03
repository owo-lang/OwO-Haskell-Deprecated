{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}

module OwO.TypeChecking.Builtin
  ( typeText
  , infText

  , builtinDefinition
  , typeUniverseModule
  , primitiveModule
  ) where

import           Data.Char
import qualified Data.Text            as T

import           OwO.Syntax.Abstract
import           OwO.Syntax.Module
import           OwO.Syntax.TokenType (Name (..), textOfName)

#include <impossible.h>

typeText = "Type" :: T.Text
typeTextSize = T.length typeText
infText = "Inf" :: T.Text

-- | Because there're infinite `TypeN`s, no way of pre-putting them into a module
--   However, we can add the used `TypeN`s lazily to the built-in module
builtinDefinition :: Name -> Maybe AstTerm
builtinDefinition name
  | typeText `T.isPrefixOf` t = let trailing = T.drop typeTextSize t in if
    | T.null trailing        -> Just . AstTypeLit name $ ULevelLit 0
    | T.all isDigit trailing -> Just . AstTypeLit name . ULevelLit . read $ T.unpack trailing
    | trailing == infText    -> Just $ AstTypeLit name ULevelMax
    | otherwise              -> Nothing
  | otherwise = Nothing
  where t = textOfName name

-- | Module name for type universes. Like a placeholder
typeUniverseModule :: QModuleName
typeUniverseModule = primitiveModule

primitiveModule :: QModuleName
primitiveModule = QModuleName
  { moduleNameList = ["OwO", "Primitive"]
  }
