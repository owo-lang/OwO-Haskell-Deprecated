{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Only for early-stage testing
module OwO.Syntax.Parser.NaiveParser (parseTokens) where

import           Control.Applicative
    ( Alternative (..)
    , Applicative (..)
    , many
    , some
    )
import           Control.Monad
import           Data.Functor
import           Data.List                          (partition)
import qualified Data.Text                          as T

import           OwO.Syntax.Abstract
import           OwO.Syntax.Parser.NaiveCombinators
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType
import           OwO.Util.Three

-- TODO: https://github.com/dramforever/each/pull/1
import           Each                               hiding ((~!))
infixl 0 ~!
(~!) = undefined

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

nameP :: Parser Name
nameP = uncurry Name <$> identifierP'

identifierP :: Parser PsiTerm
identifierP = PsiReference <$> nameP

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
--------------------------------- Fixity info ----------------------------------
--------------------------------------------------------------------------------

fixityP :: Parser PsiFixityInfo
fixityP = $(each [|
  (~! infixLP <|> infixRP <|> infixP)
  (~! fromInteger . snd <$> integerP')
  (~! some nameP) |])
  where
    infixLP = exactly InfixLToken >> return PsiInfixL
    infixRP = exactly InfixRToken >> return PsiInfixR
    infixP  = exactly InfixToken  >> return PsiInfix

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
postulateP fix = do
  exactly PostulateToken
  ts <- layoutP $ typeSignatureP' fix
  return $ uncurry3 PsiPostulate <$> ts

patternP :: [PsiFixityInfo] -> DeclarationP
patternP fix = do
  i <- identifierP'
  -- TODO patterns
  exactly EqualToken
  t <- expressionP fix
  -- TODO pragma
  return [PsiPattern (uncurry Name i) [] [] t]

declarationP :: [PsiFixityInfo] -> DeclarationP
declarationP fix = moduleP fix
  <|> postulateP fix
  <|> typeSignatureP fix
  <|> patternP fix
--  <|> return __TODO__

moduleP :: [PsiFixityInfo] -> DeclarationP
moduleP fix = do
  (name, decls) <- moduleP' fix
  let moduleName = QModuleName { moduleNameList = name }
  return [PsiSubmodule moduleName decls]

moduleP' :: [PsiFixityInfo] -> Parser ([T.Text], [PsiDeclaration])
moduleP' fix = do
  exactly ModuleToken
  modName <- exactly DotToken \||/ identifierP'
  exactly WhereToken
  content <- join <$> layoutP (declarationP fix)
  return (snd <$> modName, content)
