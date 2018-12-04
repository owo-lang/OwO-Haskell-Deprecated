{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

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

type FixityInfo = [PsiFixityInfo]
type DeclarationP = FixityInfo -> Parser (FixityInfo, [PsiDeclaration])

parseTokens ::
  FixityInfo ->
  PsiFileType ->
  [PsiToken] ->
  Either String PsiFile
parseTokens fix fType = parseCode $ do
  (name, fixity, decls) <- moduleP' fix
  let moduleName = QModuleName { moduleNameList = name }
  return $ PsiFile
    { fileType           = fType
    , topLevelModuleName = moduleName
    , declarations       = decls
    , exposedFixityInfo  = fixity
    }

semicolon :: Parser PsiToken
semicolon = exactly SemicolonToken

insideParen :: Parser a -> Parser a
insideParen p =
  exactly ParenthesisLToken
    *> p <*
  exactly ParenthesisRToken

insideBrace :: Parser a -> Parser a
insideBrace p =
  exactly BraceLToken
    *> p <*
  exactly BraceRToken

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

atomP :: FixityInfo -> Parser PsiTerm
atomP fix = identifierP
  <|> integerP
  <|> stringP
  <|> charP
  <|> insideParen (expressionP fix)

applicationP :: FixityInfo -> Parser PsiTerm
applicationP fix = atomP fix `chainl1` pure PsiApplication

expressionP :: FixityInfo -> Parser PsiTerm
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
  FixityInfo ->
  (T.Text -> Bool) ->
  Parser PsiImplInfo
patternMatchingClauseP fix f = $(each [|
  PsiImplSimple
  (~! specificNameP f)
  (~! many $ expressionP fix)
  []
  (~! exactly EqualToken *> expressionP fix)
  (~! option0 [] $ snd <$> whereClauseP fix) |])

whereClauseP :: DeclarationP
whereClauseP fix = do
  many semicolon
  exactly WhereToken
  many semicolon
  declarationsP fix

implementationP :: DeclarationP
implementationP fix = do
  ps <- many $ fnPragmaP fix <* semicolon
  hd <- patternMatchingClauseP fix $ const True
  let functionName = functionNameOfImplementation hd
  tl <- many $ semicolon *>
     patternMatchingClauseP fix (textOfName functionName ==)
  return $ (fix,) [PsiImplementation functionName ps $ hd :| tl]

--------------------------------------------------------------------------------
--------------------------------- Declarations ---------------------------------
--------------------------------------------------------------------------------

fnPragmaP :: FixityInfo -> Parser FnPragma
fnPragmaP _ = empty -- TODO

dataPragmaP :: FixityInfo -> Parser DataPragma
dataPragmaP _ = empty -- TODO

typeSignatureP' :: FixityInfo -> Parser (Name, FnPragmas, PsiTerm)
typeSignatureP' fix = do
  p <- many $ fnPragmaP fix <* semicolon
  $(each [|
    ( uncurry Name (~! identifierP')
    , p
    , (~! exactly ColonToken *> expressionP fix)
    ) |])

typeSignatureP :: DeclarationP
typeSignatureP fix = ((fix,) <$>) $
  pure . uncurry3 PsiTypeSignature <$> typeSignatureP' fix

layoutP :: Parser a -> Parser [a]
layoutP p =
  exactly BraceLToken *>
    option0 [] (semicolon \|/ p)
  <* exactly BraceRToken

layoutStatedP :: st -> (st -> Parser (st, a)) -> Parser (st, [a])
layoutStatedP st p =
  exactly BraceLToken *>
    option0 (st, []) (separatedStateful p semicolon st)
  <* exactly BraceRToken

postulateP :: DeclarationP
postulateP fix = exactly PostulateToken >> ((fix,) <$>)
  $(each [| uncurry3 PsiPostulate <$> bind (layoutP $ typeSignatureP' fix) |])

declarationP :: DeclarationP
declarationP fix = moduleP fix
  <|> postulateP fix
  <|> typeSignatureP fix
  <|> implementationP fix
--  <|> return __TODO__

declarationsP :: DeclarationP
declarationsP fix = (join <$>) <$> layoutStatedP fix declarationP

moduleP :: DeclarationP
moduleP fix = do
  (name, fixity, decls) <- moduleP' fix
  let moduleName = QModuleName { moduleNameList = name }
  -- Do not put operator info in submodules to parent modules
  -- But store them separately in the submodules
  return $ (fix,) [PsiSubmodule moduleName fixity decls]

moduleP' :: FixityInfo -> Parser ([T.Text], FixityInfo, [PsiDeclaration])
moduleP' fix = $(each [|
  flattenSnd
  ( snd <$>
    (~! exactly ModuleToken *>
          (exactly DotToken \|/ identifierP')
        <* exactly WhereToken)
  , (~! declarationsP fix)
  ) |])
