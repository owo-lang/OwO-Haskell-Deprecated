{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

-- | Only for early-stage testing
module OwO.Syntax.Parser.NaiveParser (parseTokens) where

import           Control.Applicative
    ( Alternative (..)
    , Applicative (..)
    , many
    , some
    )
import           Control.Monad
import qualified Data.Text                          as T

import           OwO.Syntax.Abstract
import           OwO.Syntax.Parser.NaiveCombinators
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType
import           OwO.Util.Three

#include <impossible.h>

type DeclarationP = Parser [PsiDeclaration]

parseTokens :: [PsiFixityInfo] -> PsiFileType -> [PsiToken] -> Either String PsiFile
parseTokens fix fType = parseCode $ do
  (name, decls) <- moduleP' fix
  let moduleName = QModuleName { moduleNameList = name }
  return $ PsiFile
    { fileType           = fType
    , topLevelModuleName = moduleName
    , declarations       = decls
    }

--------------------------------------------------------------------------------
---------------------------------- Expressions ---------------------------------
--------------------------------------------------------------------------------

identifierP' :: Parser (Loc, T.Text)
identifierP' = satisfyMap $ \tok -> case tokenType tok of
  (IdentifierToken t) -> Just (location tok, t)
  _                   -> Nothing

identifierP :: Parser PsiTerm
identifierP = PsiReference . uncurry Name <$> identifierP'

integerP' :: Parser (Loc, Integer)
integerP' = satisfyMap $ \tok -> case tokenType tok of
  (IntegerToken i) -> Just (location tok, i)
  _                -> Nothing

integerP :: Parser PsiTerm
integerP = mapInteger <$> integerP'
  where mapInteger (l, i) = PsiConstant l $ IntegerConst i

stringP' :: Parser (Loc, T.Text)
stringP' = satisfyMap $ \tok -> case tokenType tok of
  (StringToken s) -> Just (location tok, s)
  _               -> Nothing

stringP :: Parser PsiTerm
stringP = mapString <$> stringP'
  where mapString (l, s) = PsiConstant l $ StringConst s

charP' :: Parser (Loc, Char)
charP' = satisfyMap $ \tok -> case tokenType tok of
  (CharToken c) -> Just (location tok, c)
  _             -> Nothing

charP :: Parser PsiTerm
charP = mapChar <$> charP'
  where mapChar (l, c) = PsiConstant l $ CharConst c

atomP :: [PsiFixityInfo] -> Parser PsiTerm
atomP fix = identifierP
 <|> integerP
 <|> stringP
 <|> charP
 <|> do
   exactly BracketLToken
   expr <- expressionP fix
   exactly BracketRToken
   return expr

expressionP :: [PsiFixityInfo] -> Parser PsiTerm
expressionP = atomP -- TODO add other

--------------------------------------------------------------------------------
--------------------------------- Declarations ---------------------------------
--------------------------------------------------------------------------------

typeSignatureP' :: [PsiFixityInfo] -> Parser (Name, FnPragmas, PsiTerm)
typeSignatureP' fix = do
  i <- identifierP'
  exactly ColonToken
  t <- expressionP fix
  return (uncurry Name i, [], t) -- TODO pragma

typeSignatureP :: [PsiFixityInfo] -> DeclarationP
typeSignatureP fix = return . uncurry3 PsiTypeSignature <$> typeSignatureP' fix

layoutP :: Parser a -> Parser [a]
layoutP p = do
  exactly BraceLToken
  content <- option0 [] $ exactly SemicolonToken \||/ p
  exactly BraceRToken
  return content

postulateP :: [PsiFixityInfo] -> DeclarationP
postulateP fix = return . uncurry3 PsiPostulate <$> do
  exactly PostulateToken
  typeSignatureP' fix

declarationP :: [PsiFixityInfo] -> DeclarationP
declarationP fix = moduleP fix
  <|> postulateP fix
  <|> typeSignatureP fix
--  <|> return __TODO__

moduleP :: [PsiFixityInfo] -> DeclarationP
moduleP fix = do
  (name, decls) <- moduleP' fix
  let moduleName = QModuleName { moduleNameList = name }
  return . return $ PsiSubmodule moduleName decls

moduleP' :: [PsiFixityInfo] -> Parser ([T.Text], [PsiDeclaration])
moduleP' fix = do
  exactly ModuleToken
  modName <- exactly DotToken \||/ identifierP'
  exactly WhereToken
  content <- join <$> layoutP (declarationP fix)
  return (snd <$> modName, content)
