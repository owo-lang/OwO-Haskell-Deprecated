{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE ViewPatterns     #-}

-- | Should not be used in production environment
--   For early stage testing purpose
module OwO.Syntax.Parser.NaiveCombinators
 ( Parser(..)
 , (<~>)
 , parseCode

 -- Unit combinators
 , item
 , satisfy
 , dissatisfy
 , exactly
 , oneOf
 , noneOf

 -- Optional combinators
 , option0
 , option1

 -- Chained combinators
 , chainl
 , chainr
 , chainl1
 , chainr1
 , chainl2
 , separated
 , separated'
 , (\|/)
 , (\||/)
 ) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List

import           OwO.Syntax.TokenType

newtype Parser a = Parser
  { runParser :: [PsiToken] -> [(a, [PsiToken])]
  }

parseCode :: Parser a -> [PsiToken] -> Either String a
parseCode m (runParser m -> [(res, [])]) = Right res
parseCode _ _                            = Left "Yamete kudasai, senpai!"

instance Functor Parser where
  fmap f (Parser ps) = Parser $ \p -> [ (f a, b) | (a, b) <- ps p ]

instance Applicative Parser where
  pure = return
  (Parser p1) <*> (Parser p2) = Parser $ \p ->
    [ (f a, s2) | (f, s1) <- p1 p, (a, s2) <- p2 s1 ]

instance Monad Parser where
  return a = Parser $ \s -> [(a, s)]
  p >>= f  = Parser $ concatMap (\(a, s1) -> f a `runParser` s1) . runParser p

instance MonadPlus Parser where
  mzero     = Parser $ const []
  mplus p q = Parser $ \s -> runParser p s ++ runParser q s

instance Alternative Parser where
  empty   = mzero
  p <|> q = Parser $ \s -> case runParser p s of
    [] -> runParser q s
    rs -> rs

(<~>) :: Alternative a => a b -> a b -> a b
(<~>) = flip (<|>)

infixl 2 \|/
infixl 2 \||/
(\|/) = flip separated
(\||/) = flip separated'

item :: Parser PsiToken
item = Parser $ \case
  [     ] -> [      ]
  (h : t) -> [(h, t)]

satisfy :: (PsiToken -> Bool) -> Parser PsiToken
satisfy p = item >>= \c -> if p c then return c else empty

dissatisfy :: (PsiToken -> Bool) -> Parser PsiToken
dissatisfy p = satisfy $ not . p

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= restOf p op

restOf :: Parser a -> Parser (b -> a -> b) -> b -> Parser b
restOf p op a = return a <~> do
  f <- op
  p >>= restOf p op . f a

chainl2 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl2 p op = do
  a <- p
  f <- op
  p >>= restOf p op . f a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan   = p >>= rest
    rest a = return a <~> do
      f <- op
      scan >>= (rest . f a)

option1 :: Parser a -> Parser (a -> a -> a) -> Parser a
option1 p op = do
  a <- p
  return a <~> do
    f <- op
    f a <$> p

separated :: Parser a -> Parser a -> Parser [a]
separated ns ss = do
  n <- ns
  return [n] <~> do
    s <- ss
    r <- separated ns ss
    return $ n : s : r

separated' :: Parser a -> Parser b -> Parser [a]
separated' ns ss = do
  n <- ns
  return [n] <~> do
    ss
    r <- separated' ns ss
    return $ n : r

option0 :: b -> Parser b -> Parser b
option0 d = (<|> return d)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op = (chainl1 p op <|>) . return

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op = (chainr1 p op <|>) . return

exactly :: TokenType -> Parser PsiToken
exactly t = satisfy $ (== t) . tokenType

oneOf :: [TokenType] -> Parser PsiToken
oneOf ts = satisfy $ (`elem` ts) . tokenType

noneOf :: [TokenType] -> Parser PsiToken
noneOf ts = dissatisfy $ (`elem` ts) . tokenType
