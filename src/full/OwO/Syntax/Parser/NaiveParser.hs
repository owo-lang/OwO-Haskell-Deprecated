{-# LANGUAGE CPP #-}
module OwO.Syntax.Parser.NaiveParser where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text                      as T

import           OwO.Syntax.Abstract
import           OwO.Syntax.Parser.NaiveParserC
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType

#include <impossible.h>

fileP :: Parser ([T.Text], [PsiDeclaration])
fileP = do
  exactly ModuleToken
  modName <- exactly DotToken \|/ satisfy isIdentifier
  exactly WhereToken
  return (getIdentifier . tokenType <$> modName, [])
  where
    isIdentifier n = case tokenType n of
      (IdentifierToken _) -> True
      _                   -> False

    getIdentifier (IdentifierToken i) = i
    getIdentifier _                   = T.pack __IMPOSSIBLE__
