module OwO.Syntax.Parser
  ( scanAll
  , lex
  , lexFile
  -- Lexer

  , parseNaive
  , parseFileNaive
  , parseNaiveWith
  -- Parser
  ) where

import           Prelude                            hiding (lex)

import           OwO.Syntax.Concrete
import           OwO.Syntax.Parser.Alex             (AlexUserState (..))
import           OwO.Syntax.Parser.Lexer
import           OwO.Syntax.Parser.NaiveCombinators as NC
import           OwO.Syntax.Parser.NaiveParser      as NP
import           OwO.Syntax.Position                (SrcFile (..))
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

-- | Returning error message or a list of tokens
--   with file name specified
lexFile :: SrcFile -> String -> Either String [PsiToken]
lexFile file = flip runAlex $ do
  s <- alexGetUserState
  alexSetUserState s { currentFile = file }
  scanAll

parseNaive :: PsiFileType -> String -> Either String PsiFile
parseNaive t s = lex s >>= NP.parseTokens [] t

parseFileNaive :: SrcFile -> PsiFileType -> String -> Either String PsiFile
parseFileNaive src t s = lexFile src s >>= NP.parseTokens [] t

parseNaiveWith :: NC.Parser a -> String -> Either String a
parseNaiveWith p s = lex s >>= NC.parseCode p
