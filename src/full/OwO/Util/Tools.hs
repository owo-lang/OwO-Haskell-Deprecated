{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module OwO.Util.Tools
  ( dumpTokens
  , dumpAst
  , parseNaiveSimple
  , printExpr
  ) where

import           Control.Monad        (join)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import           Prelude              hiding (lex)
import           System.Exit          (exitFailure)
import           System.IO

import           OwO.Syntax.Concrete
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
simpleToken token = case tokenType token of
  OperatorToken   n -> "Operator "   ++ T.unpack (textOfName n)
  IdentifierToken n -> "Identifier " ++ T.unpack (textOfName n)
  others            -> show others

parseNaiveSimple :: String -> Either String PsiFile
parseNaiveSimple = parseNaive CodeFileType

dumpTokens :: FilePath -> Bool -> IO ()
dumpTokens file hideLocation = lex <$> readFile file >>= \case
    Left  errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right tokens ->
      let f = if hideLocation then simpleToken else prettyToken
      in mapM_ putStrLn $ f <$> tokens

put :: Int -> String -> IO ()
put indent = putStrLn . (replicate indent ' ' ++)

printName locate n = (' ' :) $
  T.unpack (textOfName n) ++ locate (locationOfName n)

printExpr :: Int -> Bool -> PsiTerm -> IO ()
printExpr indent hideLocation = \case
    PsiConstant l info -> do
      puts $ "constant" ++ locate l
      succ indent `put` show info
    PsiReference n -> puts $ "named reference" ++ name n
    PsiTelescope n vis l r -> do
      puts $ "telescope binding" ++ name n
      puts $ show vis
      puts "binding type"
      recur l
      puts "return type"
      recur r
    PsiApplication f a -> do
      puts "function application"
      puts "function being applied"
      recur f
      puts "value applied to the function"
      recur a
    _ -> __TODO__
  where
    puts   = put indent
    recur  = flip printExpr hideLocation $ succ indent
    name   = printName locate
    locate | hideLocation = const []
           | otherwise    = (' ' :) . showLoc

printImplementation :: Int -> Bool -> PsiImplInfo -> IO ()
printImplementation indent hideLocation = \case
  PsiImplSimple n app ws expr whereClause -> do
    puts $ "clause for " ++ name n
    puts "patterns matching parsed as expression"
    pExpr app
    puts "function body"
    pExpr expr
    puts $ if null ws then "no with abstraction" else pure __TODO__
    if null whereClause then puts "no where clause" else do
      puts "where clause"
      mapM_ pDecl whereClause
  where
    puts   = put indent
    recur  = flip printImplementation hideLocation $ succ indent
    pExpr  = flip printExpr hideLocation $ succ indent
    pDecl  = flip printDeclaration hideLocation $ succ indent
    name   = printName locate
    locate | hideLocation = const []
           | otherwise    = (' ' :) . showLoc

printDeclaration :: Int -> Bool -> PsiDeclaration -> IO ()
printDeclaration indent hideLocation = \case
    PsiPostulate n ps t -> do
      puts $ "postulate" ++ name n
      puts $ if null ps then " no pragmas" else pure __TODO__
      pExpr t
    PsiTypeSignature n ps t -> do
      puts $ "type signature" ++ name n
      puts $ if null ps then " no pragmas" else pure __TODO__
      pExpr t
    PsiImplementation n ps impls -> do
      puts $ "implementation of function " ++ name n
      puts $ if null ps then " no pragmas" else pure __TODO__
      mapM_ pImpl impls
    PsiSubmodule n fs ds -> do
      puts $ "submodule " ++ show n
      let printFix (n, i, ns) = do
            puts $ n ++ " " ++ show i
            mapM_ (puts . name) ns
      mapM_ (printFix . fixityInfo) fs
      mapM_ recur ds
  where
    puts   = put indent
    pExpr  = flip printExpr hideLocation $ succ indent
    pImpl  = flip printImplementation hideLocation $ succ indent
    recur  = flip printDeclaration hideLocation $ succ indent
    name   = printName locate
    locate | hideLocation = const []
           | otherwise    = (' ' :) . showLoc

dumpAst :: FilePath -> Bool -> IO ()
dumpAst file hideLocation = parseNaive ft <$> readFile file >>= \case
    Left errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right pFile -> do
      putStrLn $ "File type: "       ++ show (fileType pFile)
      putStrLn $ "Top module name: " ++ show (topLevelModuleName pFile)
      mapM_ (printDeclaration 1 hideLocation) $ declarations pFile
  where
    ft = fromMaybe CodeFileType $ decideFileType file
