module OwO.Syntax.Parser where

import           Prelude                            hiding (lex)

import           OwO.Syntax.Abstract
import           OwO.Syntax.Parser.Lexer
import           OwO.Syntax.Parser.NaiveCombinators as NC
import           OwO.Syntax.Parser.NaiveParser      as NP
import           OwO.Syntax.TokenType

scanAll :: Alex [PsiToken]
scanAll = do
  t <- alexMonadScan
  case tokenType t of
    EndOfFileToken -> pure []
    _              -> (t :) <$> scanAll

-- | Returning error message or a list of tokens
lex :: String -> Either String [PsiToken]
lex = flip runAlex scanAll

parseNaive :: PsiFileType -> String -> Either String PsiFile
parseNaive t s = lex s >>= NP.parseTokens [] t

runParser :: NC.Parser a -> String -> Either String a
runParser p s = lex s >>= NC.parseCode p
