{-# LANGUAGE LambdaCase #-}

module OwO.Util.Tools
  ( dumpTokens
  , dumpPsi
  , dumpAst
  , parseNaiveSimple
  , printExpr
  ) where

import           Data.Maybe           (fromMaybe)
import qualified Data.Map             as Map
import           Prelude              hiding (lex)
import           System.Exit          (exitFailure)
import           System.IO

import           OwO.Syntax.Context   (Context (..))
import           OwO.Syntax.Concrete
    ( PsiFile (..)
    , PsiFileType (..)
    , decideFileType
    )
import           OwO.Syntax.Parser
import           OwO.Syntax.TokenType
import           OwO.TypeChecking.Desugar
import           OwO.Util.Dump

parseNaiveSimple :: String -> Either String PsiFile
parseNaiveSimple = parseNaive CodeFileType

dumpTokens :: FilePath -> Bool -> IO ()
dumpTokens file hideLocation = lex <$> readFile file >>= \case
    Left  errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right tokens ->
      let f = if hideLocation then simpleToken else prettyToken
      in mapM_ putStrLn $ f <$> tokens

getPsiFileOrDie :: FilePath -> Bool -> IO PsiFile
getPsiFileOrDie file hideLocation = parseNaive ft <$> readFile file >>= \case
    Left errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right pFile -> return pFile
  where
    ft = fromMaybe CodeFileType $ decideFileType file

dumpPsi :: FilePath -> Bool -> IO ()
dumpPsi file hideLocation = do
  pFile <- getPsiFileOrDie file hideLocation
  putStrLn $ "File type: "       ++ show (fileType pFile)
  putStrLn $ "Top module name: " ++ show (topLevelModuleName pFile)
  mapM_ (printDeclaration 1 hideLocation) $ declarations pFile

dumpAst :: FilePath -> Bool -> IO ()
dumpAst file hideLocation = do
  pFile <- getPsiFileOrDie file hideLocation
  putStrLn $ "File type: "       ++ show (fileType pFile)
  putStrLn $ "Top module name: " ++ show (topLevelModuleName pFile)
  case concreteToAbstractDecl $ declarations pFile of
    Left errMsg -> print errMsg
    Right decls -> mapM_ (printDeclarationAst 1 hideLocation) $
      Map.elems $ localCtx decls
