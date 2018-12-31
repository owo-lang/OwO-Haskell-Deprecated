module Main where

-- import           Control.Monad.IO.Class        (liftIO)
import           Data.Either                   (isLeft, isRight)
import qualified Data.Text                     as T

import           Test.Hspec

import           System.Exit                   (ExitCode (..), exitWith)

import           OwO.Syntax.Concrete
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

  describe "Simple reference resolving" $ do
    let pre = parseNaiveWith $ snd <$> NP.declarationsP []
        p   = (concreteToAbstractDecl <$>) . pre

    it "Should resolve built-in definitions" $ do
      p "{ a = Type }" `shouldSatisfy` isRight
      p "{ a = Type; b = a }" `shouldSatisfy` isRight
      p "{ id = \\x -> x }" `shouldSatisfy` isRight
      p "{ idType = Type -> Type }" `shouldSatisfy` isRight

    it "Should give unresolved-reference" $ do
      let notOk (Right (Left (UnresolvedReferenceError _))) = True
          notOk _ = False
      p "{ a = b }" `shouldSatisfy` notOk
      p "{ a : b }" `shouldSatisfy` notOk
      p "{ id = \\x -> y }" `shouldSatisfy` notOk

    it "Should give unimplemented errors" $ do
      let notOk (Right (Left (NoImplementationError _))) = True
          notOk _ = False
      p "{ a : Type }" `shouldSatisfy` notOk
      p "{ id : Type -> Type }" `shouldSatisfy` notOk

  describe "Infix declaration parsing" $ do

    it "Should not parse incorrect infix declarations" $ do
      let p = parseNaiveWith NP.fixityP
      p "infixl 1 233"  `shouldSatisfy` isLeft
      p "infixr +"      `shouldSatisfy` isLeft
      p "infix 1"       `shouldSatisfy` isLeft

    it "Should parse simple infix declarations" $ do
      let p = parseNaiveWith NP.fixityP
      p "infixl 1 +"  `shouldSatisfy` isRight
      p "infixr 2 -"  `shouldSatisfy` isRight
      p "infix 3 <|>" `shouldSatisfy` isRight

  describe "Type signature parsing" $ do

    it "Should not parse errored files" $ do
      let p = parseNaiveSimple
      p "module A where\na :" `shouldSatisfy` isLeft
      p "module B where\n: b" `shouldSatisfy` isLeft

    it "Should parse simple type signatures" $ do
      let p = parseNaiveSimple
      p "module A where\na : b" `shouldSatisfy` isRight
      p "module A where\na:b"   `shouldSatisfy` isRight

  describe "Module name parsing" $ do

    it "Should not work for errored files" $ do
      let p = parseNaiveSimple
      p ""             `shouldSatisfy` isLeft
      p "module"       `shouldSatisfy` isLeft
      p "module where" `shouldSatisfy` isLeft
      p "where"        `shouldSatisfy` isLeft

    it "Should parse module name" $ do
      let p s = moduleNameList . topLevelModuleName <$> parseNaiveSimple s
      p "module A where\n"     `shouldBe` Right [T.pack "A"]
      p "module A.B where\n"   `shouldBe` Right (T.pack <$> ["A", "B"])
      p "module A.B.C where\n" `shouldBe` Right (T.pack <$> ["A", "B", "C"])
