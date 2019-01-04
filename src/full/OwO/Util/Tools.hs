{-# LANGUAGE LambdaCase #-}

module OwO.Util.Tools
  ( dumpTokens
  , dumpPsi
  , dumpAst
  , parseNaiveSimple
  , printExpr
  , getDecls
  ) where

import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import           Prelude                  hiding (lex)
import           System.Exit              (exitFailure)
import           System.IO

import           OwO.Syntax.Concrete
    ( PsiDeclaration (..)
    , PsiFile (..)
    , PsiFileType (..)
    , decideFileType
    )
import           OwO.Syntax.Context       (Context (..))
import           OwO.Syntax.Parser
import           OwO.Syntax.TokenType
import           OwO.TypeChecking.Desugar
import           OwO.Util.Dump
import qualified OwO.Util.StrictMaybe     as Strict

parseNaiveSimple :: String -> Either String PsiFile
parseNaiveSimple = parseNaive CodeFileType

dumpTokens :: FilePath -> Bool -> IO ()
dumpTokens file hideLocation = lexFile src <$> readFile file >>= \case
  Left  errMsg -> hPutStrLn stderr errMsg >> exitFailure
  Right tokens ->
    let f = if hideLocation then simpleToken else prettyToken
    in mapM_ putStrLn $ f <$> tokens
  where
    src = Strict.Just $ T.pack file

getPsiFileOrDie :: FilePath -> Bool -> IO PsiFile
getPsiFileOrDie file hideLocation =
  parseFileNaive src ft <$> readFile file >>= \case
    Left errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right pFile -> return pFile
  where
    ft = fromMaybe CodeFileType $ decideFileType file
    src = Strict.Just $ T.pack file

getDecls :: FilePath -> Bool -> IO [PsiDeclaration]
getDecls file hideLocation = do
  pFile <- getPsiFileOrDie file hideLocation
  putStrLn $ "File type: "       ++ show (fileType pFile)
  putStrLn $ "Top module name: " ++ show (topLevelModuleName pFile)
  return $ declarations pFile

dumpPsi :: FilePath -> Bool -> IO ()
dumpPsi file hideLocation = getDecls file hideLocation >>=
  mapM_ (printDeclaration 1 hideLocation)

dumpAst :: FilePath -> Bool -> IO ()
dumpAst file hideLocation = do
  decls <- getDecls file hideLocation
  case concreteToAbstractDecl decls of
    Left errMsg -> print errMsg
    Right decls -> mapM_ (printDeclarationAst 1 hideLocation) .
      Map.elems $ localCtx decls
