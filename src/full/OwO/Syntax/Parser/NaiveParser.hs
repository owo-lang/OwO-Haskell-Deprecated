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
import           Data.List.NonEmpty                 (NonEmpty (..))
import qualified Data.Text                          as T

import           Each

import           OwO.Syntax.Abstract
import           OwO.Syntax.Parser.NaiveCombinators
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType
import           OwO.Util.Three

type DeclarationP = Parser [PsiDeclaration]

parseTokens ::
  [PsiFixityInfo] ->
  PsiFileType ->
  [PsiToken] ->
  Either String PsiFile
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
  IdentifierToken t -> Just (location tok, t)
  _                 -> Nothing

specificNameP :: (T.Text -> Bool) -> Parser Name
specificNameP f = satisfyMap $ \tok -> case tokenType tok of
  IdentifierToken t -> if f t then Just . flip Name t $ location tok
                       else Nothing
  _                 -> Nothing

nameP :: Parser Name
nameP = uncurry Name <$> identifierP'

identifierP :: Parser PsiTerm
identifierP = PsiReference <$> nameP

integerP' :: Parser (Loc, Integer)
integerP' = satisfyMap $ \tok -> case tokenType tok of
  IntegerToken i -> Just (location tok, i)
  _              -> Nothing

integerP :: Parser PsiTerm
integerP = uncurry ((. IntegerConst) . PsiConstant) <$> integerP'

stringP' :: Parser (Loc, T.Text)
stringP' = satisfyMap $ \tok -> case tokenType tok of
  StringToken s -> Just (location tok, s)
  _             -> Nothing

stringP :: Parser PsiTerm
stringP = uncurry ((. StringConst) . PsiConstant) <$> stringP'

charP' :: Parser (Loc, Char)
charP' = satisfyMap $ \tok -> case tokenType tok of
  CharToken c -> Just (location tok, c)
  _           -> Nothing

charP :: Parser PsiTerm
charP = uncurry ((. CharConst) . PsiConstant) <$> charP'

atomP :: [PsiFixityInfo] -> Parser PsiTerm
atomP fix = identifierP
  <|> integerP
  <|> stringP
  <|> charP
  <|> exactly ParenthesisLToken *>
       expressionP fix
      <* exactly ParenthesisRToken

applicationP :: [PsiFixityInfo] -> Parser PsiTerm
applicationP fix = atomP fix `chainl1` pure PsiApplication

expressionP :: [PsiFixityInfo] -> Parser PsiTerm
expressionP fix = applicationP fix
  <|> atomP fix -- TODO add other

--------------------------------------------------------------------------------
---------------------------------- Operators -----------------------------------
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
----------------------------- Function definitions -----------------------------
--------------------------------------------------------------------------------

-- TODO: support with abstraction
patternMatchingClauseP ::
  [PsiFixityInfo] ->
  (T.Text -> Bool) ->
  Parser PsiImplInfo
patternMatchingClauseP fix f = $(each [|
  PsiImplSimple
  (~! specificNameP f)
  (~! many $ expressionP fix)
  []
  (~! exactly EqualToken *> expressionP fix)
  (~! option0 [] $ whereClauseP fix) |])

whereClauseP :: [PsiFixityInfo] -> DeclarationP
whereClauseP fix = do
  many $ exactly SemicolonToken
  exactly WhereToken
  many $ exactly SemicolonToken
  join <$> layoutP (declarationP fix)

implementationP :: [PsiFixityInfo] -> DeclarationP
implementationP fix = do
  ps <- many $ fnPragmaP fix <* exactly SemicolonToken
  hd <- patternMatchingClauseP fix $ const True
  let functionName = functionNameOfImplementation hd
  tl <- many $ exactly SemicolonToken *>
     patternMatchingClauseP fix (textOfName functionName ==)
  return [PsiImplementation functionName ps $ hd :| tl]

--------------------------------------------------------------------------------
--------------------------------- Declarations ---------------------------------
--------------------------------------------------------------------------------

fnPragmaP :: [PsiFixityInfo] -> Parser FnPragma
fnPragmaP _ = empty -- TODO

dataPragmaP :: [PsiFixityInfo] -> Parser DataPragma
dataPragmaP _ = empty -- TODO

typeSignatureP' :: [PsiFixityInfo] -> Parser (Name, FnPragmas, PsiTerm)
typeSignatureP' fix = do
  p <- many $ fnPragmaP fix <* exactly SemicolonToken
  $(each [|
    ( uncurry Name (~! identifierP')
    , p
    , (~! exactly ColonToken *> expressionP fix)
    ) |])

typeSignatureP :: [PsiFixityInfo] -> DeclarationP
typeSignatureP fix = pure . uncurry3 PsiTypeSignature <$> typeSignatureP' fix

layoutP :: Parser a -> Parser [a]
layoutP p =
  exactly BraceLToken *>
    option0 [] (exactly SemicolonToken \||/ p)
  <* exactly BraceRToken

postulateP :: [PsiFixityInfo] -> DeclarationP
postulateP fix = exactly PostulateToken >>
  $(each [| uncurry3 PsiPostulate <$> bind (layoutP $ typeSignatureP' fix) |])

declarationP :: [PsiFixityInfo] -> DeclarationP
declarationP fix = moduleP fix
  <|> postulateP fix
  <|> typeSignatureP fix
  <|> implementationP fix
--  <|> return __TODO__

moduleP :: [PsiFixityInfo] -> DeclarationP
moduleP fix = do
  (name, decls) <- moduleP' fix
  let moduleName = QModuleName { moduleNameList = name }
  return [PsiSubmodule moduleName decls]

moduleP' :: [PsiFixityInfo] -> Parser ([T.Text], [PsiDeclaration])
moduleP' fix = $(each [|
  ( snd <$> (~! exactly ModuleToken *>
                  (exactly DotToken \||/ identifierP')
                <* exactly WhereToken)
  , (~! join <$> layoutP (declarationP fix))
  ) |])
