{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module OwO.Util.Tools
  ( dumpTokens
  , dumpAst
  , parseNaiveSimple
  ) where

import           Data.Maybe           (fromMaybe)
import           Prelude              hiding (lex)
import           System.Exit          (exitFailure)
import           System.IO

import           OwO.Syntax.Abstract
import           OwO.Syntax.Parser
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType

#include <impossible.h>

prettyToken :: PsiToken -> String
prettyToken token = simpleToken token ++ " " ++
    printLoc (iStart loc) ++ " " ++ printLoc (iEnd loc)
  where
    loc = location token
    printLoc loc = "(" ++ show (posPos  loc) ++
                   " " ++ show (posLine loc) ++
                   " " ++ show (posCol  loc) ++
                   ")"

simpleToken :: PsiToken -> String
simpleToken = show . tokenType

parseNaiveSimple :: String -> Either String PsiFile
parseNaiveSimple = parseNaive CodeFileType

dumpTokens :: FilePath -> Bool -> IO ()
dumpTokens file hideLocation = lex <$> readFile file >>= \case
    Left  errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right tokens ->
      let f = if hideLocation then simpleToken else prettyToken
      in mapM_ putStrLn $ f <$> tokens

printAst :: Bool -> PsiDeclaration -> IO ()
printAst hideLocation declaration = return __TODO__

dumpAst :: FilePath -> Bool -> IO ()
dumpAst file hideLocation = parseNaive ft <$> readFile file >>= \case
    Left errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right pFile -> do
      putStrLn $ "File type: "       ++ show (fileType pFile)
      putStrLn $ "Top module name: " ++ show (topLevelModuleName pFile)
      mapM_ (printAst hideLocation) $ declarations pFile
  where
    ft = fromMaybe CodeFileType $ decideFileType file
