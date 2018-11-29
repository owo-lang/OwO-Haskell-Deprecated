module Main where

import           Data.Either         (isLeft, isRight)
import qualified Data.Text           as T

import           Test.Hspec

import           System.Exit         (ExitCode (..), exitWith)

import           OwO.Syntax.Abstract
import           OwO.Util.Tools

checkExit :: ExitCode -> IO ()
checkExit ExitSuccess = pure ()
checkExit n           = exitWith n

main :: IO ()
main = hspec $ do

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
