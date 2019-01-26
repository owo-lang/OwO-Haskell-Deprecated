{-# LANGUAGE FlexibleContexts #-}
module Main where

-- import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Except          (runExceptT)
import           Control.Monad.State           (runStateT)
import           Control.Monad.Identity        (runIdentity)
import           Data.Either                   (isLeft, isRight)
import qualified Data.Text                     as T
import qualified Data.Map                      as Map

import           Test.Hspec

import           System.Exit                   (ExitCode (..), exitWith)

import           OwO.Syntax.Concrete
import           OwO.Syntax.Context            (emptyCtx)
import           OwO.Syntax.Module
import           OwO.Syntax.Parser             (parseNaiveWith)
import           OwO.Syntax.Parser.NaiveParser as NP
import           OwO.TypeChecking.Desugar
import           OwO.Util.Tools

checkExit :: ExitCode -> IO ()
checkExit ExitSuccess = pure ()
checkExit n           = exitWith n

main :: IO ()
main = hspec $ do

  describe "simple reference resolving" $ do
    let p str = do
          decls <- flip parseNaiveWith str $ snd <$> NP.declarationsP []
          let tcm = concreteToAbstractDecl decls
          runExceptT $ runStateT tcm emptyCtx

    let ok (Right (Right _)) = True
        ok _ = False

    it "should resolve built-in definitions" $ do
      p "{ a = Type }" `shouldSatisfy` ok
      p "{ id = Type -> Type }" `shouldSatisfy` ok
      p "{ id = Type1 -> Type2 }" `shouldSatisfy` ok
      p "{ id = TypeInf -> Type0 }" `shouldSatisfy` ok

    it "should resolve contextual definitions" $ do
      p "{ a = Type; b = a }" `shouldSatisfy` ok
      p "{ postulate { a : Type } ; id = a -> a }" `shouldSatisfy` ok
      p "{ id = \\x -> x }" `shouldSatisfy` ok

    it "should give unresolved-reference" $ do
      let notOk (Right (Left (UnresolvedReferenceError _))) = True
          notOk _ = False
      p "{ a = b }" `shouldSatisfy` notOk
      p "{ a : b }" `shouldSatisfy` notOk
      p "{ id = \\x -> y }" `shouldSatisfy` notOk

    it "should give unimplemented errors" $ do
      let notOk (Right (Right (NoImplementationError _ : _, _))) = True
          notOk _ = False
      p "{ a : Type }" `shouldSatisfy` notOk
      p "{ id : Type -> Type }" `shouldSatisfy` notOk

  describe "Infix declaration parsing" $ do

    it "should not parse incorrect infix declarations" $ do
      let p = parseNaiveWith NP.fixityP
      p "infixl 1 233"  `shouldSatisfy` isLeft
      p "infixr +"      `shouldSatisfy` isLeft
      p "infix 1"       `shouldSatisfy` isLeft

    it "should parse simple infix declarations" $ do
      let p = parseNaiveWith NP.fixityP
      p "infixl 1 +"  `shouldSatisfy` isRight
      p "infixr 2 -"  `shouldSatisfy` isRight
      p "infix 3 <|>" `shouldSatisfy` isRight

  describe "Type signature parsing" $ do

    it "should not parse errored files" $ do
      let p = parseNaiveSimple
      p "module A where\na :" `shouldSatisfy` isLeft
      p "module B where\n: b" `shouldSatisfy` isLeft

    it "should parse simple type signatures" $ do
      let p = parseNaiveSimple
      p "module A where\na : b" `shouldSatisfy` isRight
      p "module A where\na:b"   `shouldSatisfy` isRight

  describe "Module name parsing" $ do

    it "should not work for errored files" $ do
      let p = parseNaiveSimple
      p ""             `shouldSatisfy` isLeft
      p "module"       `shouldSatisfy` isLeft
      p "module where" `shouldSatisfy` isLeft
      p "where"        `shouldSatisfy` isLeft

    it "should parse module name" $ do
      let p s = moduleNameList . topLevelModuleName <$> parseNaiveSimple s
      p "module A where\n"     `shouldBe` Right [T.pack "A"]
      p "module A.B where\n"   `shouldBe` Right (T.pack <$> ["A", "B"])
      p "module A.B.C where\n" `shouldBe` Right (T.pack <$> ["A", "B", "C"])
