{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module OwO.Util.Tools
  ( dumpTokens
  , dumpAst
  , parseNaiveSimple
  ) where

import           Control.Monad        (join)
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import           Prelude              hiding (lex)
import           System.Exit          (exitFailure)
import           System.IO

import           OwO.Syntax.Abstract
import           OwO.Syntax.Parser
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType

#include <impossible.h>

prettyToken :: PsiToken -> String
prettyToken token = simpleToken token ++ " " ++ showLoc loc
  where loc = location token

showLoc :: Loc' a -> String
showLoc loc = showPos (iStart loc) ++ " " ++ showPos (iEnd loc)

showPos :: Position' a -> String
showPos p = "(" ++ show (posPos  p) ++
            " " ++ show (posLine p) ++
            " " ++ show (posCol  p) ++
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

printAst :: Int -> Bool -> PsiDeclaration -> IO ()
printAst indent hideLocation = \case
    PsiFixity info -> do
      let (n, i, ns) = fixityInfo info
      puts $ n ++ " " ++ show i
      mapM_ (puts . name) ns
    PsiPostulate n ps t -> do
      puts $ "postulate" ++ name n
      puts $ if ps == [] then " no pragmas" else __TODO__
      -- TODO print the type expression
    PsiTypeSignature n ps t -> do
      puts $ "type signature" ++ name n
      puts $ if ps == [] then " no pragmas" else __TODO__
      -- TODO print the type expression
    PsiSubmodule n ds -> do
      puts $ "submodule " ++ show n
      mapM_ recur ds
    _ -> __TODO__
  where
    puts   = putStrLn . (replicate indent ' ' ++)
    name n = (' ' :) $ T.unpack (textOfName n) ++ locate (locationOfName n)
    recur  = flip printAst hideLocation $ succ indent
    locate | hideLocation = const []
           | otherwise    = (' ' :) . showLoc

dumpAst :: FilePath -> Bool -> IO ()
dumpAst file hideLocation = parseNaive ft <$> readFile file >>= \case
    Left errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right pFile -> do
      putStrLn $ "File type: "       ++ show (fileType pFile)
      putStrLn $ "Top module name: " ++ show (topLevelModuleName pFile)
      mapM_ (printAst 1 hideLocation) $ declarations pFile
  where
    ft = fromMaybe CodeFileType $ decideFileType file
