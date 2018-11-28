{-# LANGUAGE CPP #-}
module OwO.Syntax.Parser.NaiveParser (parseTokens) where

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

parseTokens :: PsiFileType -> [PsiToken] -> Either String PsiFile
parseTokens fType = parseCode $ do
  (name, decls) <- moduleP
  let moduleName = QModuleName { moduleNameList = name }
  return $ PsiFile
    { fileType           = fType
    , topLevelModuleName = moduleName
    , declarations       = decls
    }

layoutP :: Parser a -> Parser [a]
layoutP p = do
  exactly BraceLToken
  content <- option0 [] $ exactly SemicolonToken \||/ p
  exactly BraceRToken
  return content

postulateP :: Parser [PsiDeclaration]
postulateP = do
  exactly PostulateToken
  -- TODO convert to PsiPostulate
  join <$> layoutP declarationP

declarationP :: Parser [PsiDeclaration]
declarationP = (return <$> moduleP')
  <|> postulateP
--  <|> return __TODO__

moduleP' :: Parser PsiDeclaration
moduleP' = do
  (name, decls) <- moduleP
  let moduleName = QModuleName { moduleNameList = name }
  return $ PsiSubmodule moduleName decls

moduleP :: Parser ([T.Text], [PsiDeclaration])
moduleP = do
  exactly ModuleToken
  modName <- exactly DotToken \||/ satisfy isIdentifier
  exactly WhereToken
  content <- join <$> layoutP declarationP
  return (getIdentifier . tokenType <$> modName, content)
  where
    getIdentifier (IdentifierToken i) = i
    getIdentifier _                   = T.pack __IMPOSSIBLE__

isIdentifier :: PsiToken -> Bool
isIdentifier n = case tokenType n of
  (IdentifierToken _) -> True
  _                   -> False
