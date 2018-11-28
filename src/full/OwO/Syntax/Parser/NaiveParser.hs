{-# LANGUAGE CPP #-}
module OwO.Syntax.Parser.NaiveParser where

import           Control.Applicative
    ( Alternative (..)
    , Applicative (..)
    , many
    , some
    )
import           Control.Monad
import qualified Data.Text                      as T

import           OwO.Syntax.Abstract
import           OwO.Syntax.Parser.NaiveParserC
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType

#include <impossible.h>

declarationP :: Parser PsiDeclaration
declarationP = moduleP'
  <|> return __TODO__

moduleP' :: Parser PsiDeclaration
moduleP' = do
  (name, decls) <- moduleP
  let moduleName = QModuleName { moduleNameList = name }
  PsiSubmodule moduleName decls

moduleP :: Parser ([T.Text], [PsiDeclaration])
moduleP = do
  exactly ModuleToken
  modName <- exactly DotToken \|/ satisfy isIdentifier
  exactly WhereToken
  content <- many declarationP
  return (getIdentifier . tokenType <$> modName, content)
  where
    isIdentifier n = case tokenType n of
      (IdentifierToken _) -> True
      _                   -> False

    getIdentifier (IdentifierToken i) = i
    getIdentifier _                   = T.pack __IMPOSSIBLE__
