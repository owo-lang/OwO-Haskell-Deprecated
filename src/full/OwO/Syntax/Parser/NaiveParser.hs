{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

-- | Only for early-stage testing
module OwO.Syntax.Parser.NaiveParser
  ( parseTokens
  , fixityP
  , expressionP
  ) where

import           Control.Applicative
    ( Alternative (..)
    , Applicative (..)
    , many
    , some
    )
import           Control.Monad
import           Data.Functor
import           Data.List                          (partition, sortOn)
import           Data.List.NonEmpty                 (NonEmpty (..))
import qualified Data.Text                          as T

import           Each

import           OwO.Syntax.Concrete
import           OwO.Syntax.Parser.NaiveCombinators
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType
import           OwO.Util.Three

#include <impossible.h>

type FixityInfo = [PsiFixityInfo]
type DeclarationP = FixityInfo -> Parser (FixityInfo, [PsiDeclaration])

data RegularFixityInfo a = L a | R a | No a
  deriving (Eq, Functor, Ord)

-- | Sorted list that holds all fixities
type RegularFixity a = [RegularFixityInfo a]

parseTokens ::
  FixityInfo ->
  PsiFileType ->
  [PsiToken] ->
  Either String PsiFile
parseTokens fix fType = parseCode $ do
  (name, fixity, decls) <- moduleP' fix
  let moduleName = QModuleName { moduleNameList = textOfName <$> name }
  return PsiFile
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

identifierP' :: Parser Name
identifierP' = satisfyMap $ \tok -> case tokenType tok of
  IdentifierToken t -> Just t
  _                 -> Nothing

operatorP' :: Parser Name
operatorP' = satisfyMap $ \tok -> case tokenType tok of
  OperatorToken t -> Just t
  _               -> Nothing

specificOperatorP :: (Name -> Bool) -> Parser Name
specificOperatorP f = satisfyMap $ \tok -> case tokenType tok of
  OperatorToken t -> if f t then Just t
                     else Nothing
  _               -> Nothing

identifierP :: Parser PsiTerm
identifierP = PsiReference <$> identifierP'

integerP' :: Parser (Loc, Integer)
integerP' = satisfyMap $ \tok -> case tokenType tok of
  IntegerToken i -> Just (location tok, i)
  _              -> Nothing

integerP :: Parser PsiTerm
integerP = uncurry ((. IntegerLit) . PsiLiteral) <$> integerP'

stringP' :: Parser (Loc, T.Text)
stringP' = satisfyMap $ \tok -> case tokenType tok of
  StringToken s -> Just (location tok, s)
  _             -> Nothing

stringP :: Parser PsiTerm
stringP = uncurry ((. StringLit) . PsiLiteral) <$> stringP'

charP' :: Parser (Loc, Char)
charP' = satisfyMap $ \tok -> case tokenType tok of
  CharToken c -> Just (location tok, c)
  _           -> Nothing

charP :: Parser PsiTerm
charP = uncurry ((. CharLit) . PsiLiteral) <$> charP'

atomP :: FixityInfo -> Parser PsiTerm
atomP fix = identifierP
  <|> integerP
  <|> stringP
  <|> charP
  <|> insideParen (expressionP fix)

applicationP :: FixityInfo -> Parser PsiTerm
applicationP fix = chainl1 (atomP fix) $ pure PsiApplication

lambdaP :: Parser PsiTerm -> Parser PsiTerm
lambdaP exprP = exprP <~> do
  exactly BackslashToken
  name <- identifierP'
  exactly RightArrowToken
  $(each [| PsiLambda name (~! lambdaP exprP) |])

telescopeBindingP :: Parser PsiTerm -> Parser (Name, Visibility, PsiTerm)
telescopeBindingP exprP = explicitP <|> implicitP <|> instanceP
  where
    anonymousP :: a -> Parser (Name, a, PsiTerm)
    anonymousP vis = do
      e <- exprP
      return (NoName $ locationOfTerm e, vis, e)
    bindingP :: a -> TokenType -> TokenType -> Parser (Name, a, PsiTerm)
    bindingP vis l r = anonymousP vis <~> do
      exactly l
      name <- identifierP'
      exactly ColonToken
      expr <- exprP
      exactly r
      return (name, vis, expr)
    explicitP = bindingP Explicit ParenthesisLToken ParenthesisRToken
    implicitP = bindingP Implicit BraceLToken BraceRToken
    instanceP = bindingP Instance InstanceArgumentLToken InstanceArgumentRToken

telescopeP :: Parser PsiTerm -> Parser PsiTerm
telescopeP exprP = exprP <~> do
  (name, vis, term) <- telescopeBindingP exprP
  exactly RightArrowToken
  $(each [| PsiTelescope name vis term (~! telescopeP exprP) |])

binaryExpressionP :: FixityInfo -> Parser PsiTerm
binaryExpressionP fix = operatorsP (regularizeFixity fix) $
  applicationP fix <|> atomP fix

expressionP :: FixityInfo -> Parser PsiTerm
expressionP = telescopeP . lambdaP . binaryExpressionP

--------------------------------------------------------------------------------
---------------------------------- Operators -----------------------------------
--------------------------------------------------------------------------------

fixityP :: Parser PsiFixityInfo
fixityP = $(each [|
  (~! infixLP <|> infixRP <|> infixP)
  (~! fromInteger . snd <$> integerP')
  (~! some operatorP') |])
  where
    infixLP = exactly InfixLToken $> PsiInfixL
    infixRP = exactly InfixRToken $> PsiInfixR
    infixP  = exactly InfixToken  $> PsiInfix

infixDeclarationP :: DeclarationP
infixDeclarationP fix = (, []) . (: fix) <$> fixityP

regularizeFixity :: FixityInfo -> RegularFixity [Name]
regularizeFixity = (snd <$>)
                 . sortOn fst
                 . (mapFixity <$>)
  where
    mapFixity (PsiInfixL i a) = (i, L  a)
    mapFixity (PsiInfixR i a) = (i, R  a)
    mapFixity (PsiInfix  i a) = (i, No a)

-- | Inspiration: https://www.codewars.com/kata/operator-parser
operatorsP :: RegularFixity [Name] -> Parser PsiTerm -> Parser PsiTerm
operatorsP fix rp = foldr fu rp fix

fu :: RegularFixityInfo [Name] -> Parser PsiTerm -> Parser PsiTerm
fu (L  a) atom = chainr1 atom . foldr1 (<|>) $ makeChain' <$> a
fu (R  a) atom = chainl1 atom . foldr1 (<|>) $ makeChain' <$> a
fu (No a) atom = option1 atom . foldr1 (<|>) $ makeChain' <$> a

makeChain' :: Name -> Parser (PsiTerm -> PsiTerm -> PsiTerm)
makeChain' = makeChain . (PsiReference <$>) . specificOperatorP . (==)

makeChain :: Parser PsiTerm -> Parser (PsiTerm -> PsiTerm -> PsiTerm)
makeChain = (((PsiApplication .) . PsiApplication) <$>)

--------------------------------------------------------------------------------
----------------------------- Function definitions -----------------------------
--------------------------------------------------------------------------------

-- TODO: support with abstraction
patternMatchingClauseP ::
  FixityInfo ->
  (Name -> Bool) ->
  Parser PsiImplInfo
patternMatchingClauseP fix f = do
  e <- binaryExpressionP fix
  n <- tryExtractingName e
  mkImplInfo n e
  where
    tryExtractingName = \case
      app@(PsiApplication a _)   -> tryExtractingName a
      ref@(PsiReference a) | f a -> return a
      _ -> empty
    mkImplInfo fnName expr = $(each [|
      PsiImplSimple fnName expr []
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
     patternMatchingClauseP fix (functionName ==)
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
  n <- identifierP'
  exactly ColonToken
  t <- expressionP fix
  return (n, p, t)

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
declarationP fix = foldr1 (<|>) $ ($ fix) <$>
  [ moduleP
  , postulateP
  , typeSignatureP
  , implementationP
  , implementationP
  , infixDeclarationP
  ]

declarationsP :: DeclarationP
declarationsP fix = (join <$>) <$> layoutStatedP fix declarationP

-- | I did not put operator info in submodules to parent modules
--   but store them separately in the submodules
moduleP :: DeclarationP
moduleP fix = do
  (name, fixity, decls) <- moduleP' fix
  let moduleName = QModuleName { moduleNameList = textOfName <$> name }
  return $ (fix,) [PsiSubmodule moduleName fixity decls]

moduleP' :: FixityInfo -> Parser ([Name], FixityInfo, [PsiDeclaration])
moduleP' fix = $(each [|
  flattenSnd
  ( (~! exactly ModuleToken *>
          (exactly DotToken \|/ identifierP')
        <* exactly WhereToken)
  , (~! declarationsP fix)
  ) |])
