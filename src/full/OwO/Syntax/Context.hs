{-# LANGUAGE TupleSections #-}

-- | Resolve references
module OwO.Syntax.Context where

import           Control.Applicative
import qualified Data.Map            as Map
import qualified Data.Text           as T

import           OwO.Syntax.Module

type Text = T.Text

type Binding a = Map.Map Text a

-- | Symbol table
data Context a = Context
  { externalCtx :: Map.Map QModuleName (Binding a)
  , localCtx    :: Binding a
  } deriving Show

emptyCtx :: Context a
emptyCtx = Context
  { externalCtx = Map.empty
  , localCtx    = Map.empty
  }

instance Functor Context where
  fmap f (Context external local) = Context
    { externalCtx = fmap (fmap f) external
    , localCtx    = fmap f local
    }

-- | Maybe useful for completion?
--   Dunno, LOL.
allExternalNames :: Context a -> [(QModuleName, Text)]
allExternalNames (Context ctx _) = Map.toList ctx >>=
  \ (m, ns) -> (m,) <$> Map.keys ns

-- | Lookup a definition in a known module and all its parent module
lookupCtxRecursive :: QModuleName -> Text -> Context a -> Maybe a
lookupCtxRecursive currentModule name c@(Context ctx _) =
  lookupCtxQualified currentModule name c <|>
  (parentModule currentModule >>= \m -> lookupCtxRecursive m name c)

-- | Lookup a definition in a known module and all its parent module
lookupCtxQualified :: QModuleName -> Text -> Context a -> Maybe a
lookupCtxQualified currentModule name (Context ctx _) =
  Map.lookup currentModule ctx >>= Map.lookup name

-- | Lookup a definition in current module
lookupCtxCurrent :: Text -> Context a -> Maybe a
lookupCtxCurrent name = Map.lookup name . localCtx

-- lookupCtx :: Name -> Context a -> Maybe a
-- lookupCtx name =
--   lookupCtxWithName currentModule $ textOfName name

-- -- | Overwriting
-- addDefinitionWithName :: QModuleName -> Text -> a -> Context a -> Context a
-- addDefinitionWithName targetModule name a ctx = maybe ctx
--   ((\ctx' -> Map.insert targetModule ctx' ctx) <$> Map.insert name a)
--   (Map.lookup targetModule ctx)

-- | Put local conetxt contents into external contexts,
--   clean up local context
finishModule :: QModuleName -> Context a -> Context a
finishModule currentModule (Context external local) = Context
  { externalCtx = Map.insert currentModule local external
  , localCtx    = Map.empty
  }

addDefinition :: Text -> a -> Context a -> Context a
addDefinition name a ctx =
  ctx { localCtx = Map.insert name a $ localCtx ctx }
