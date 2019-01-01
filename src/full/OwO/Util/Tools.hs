{-# LANGUAGE LambdaCase #-}

module OwO.Util.Tools
  ( dumpTokens
  , dumpPsi
  , parseNaiveSimple
  , printExpr
  ) where

import           Data.Maybe           (fromMaybe)
import           Prelude              hiding (lex)
import           System.Exit          (exitFailure)
import           System.IO

import           OwO.Syntax.Concrete
    ( PsiFile (..)
    , PsiFileType (..)
    , decideFileType
    )
import           OwO.Syntax.Parser
import           OwO.Syntax.TokenType
import           OwO.Util.Dump

parseNaiveSimple :: String -> Either String PsiFile
parseNaiveSimple = parseNaive CodeFileType

dumpTokens :: FilePath -> Bool -> IO ()
dumpTokens file hideLocation = lex <$> readFile file >>= \case
    Left  errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right tokens ->
      let f = if hideLocation then simpleToken else prettyToken
      in mapM_ putStrLn $ f <$> tokens

dumpPsi :: FilePath -> Bool -> IO ()
dumpPsi file hideLocation = parseNaive ft <$> readFile file >>= \case
    Left errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right pFile -> do
      putStrLn $ "File type: "       ++ show (fileType pFile)
      putStrLn $ "Top module name: " ++ show (topLevelModuleName pFile)
      mapM_ (printDeclaration 1 hideLocation) $ declarations pFile
  where
    ft = fromMaybe CodeFileType $ decideFileType file
