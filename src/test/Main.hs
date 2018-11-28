module Main where

import           Data.Either    (isLeft)
import qualified Data.Text      as T

import           Test.Hspec
-- import           Text.Pretty.Simple (pPrint)

import           System.Exit    (ExitCode (..), exitWith)
import           System.Process
    ( CreateProcess (..)
    , createProcess
    , proc
    , waitForProcess
    )

import           OwO.Syntax.Abstract
import           OwO.Util.Tools

checkExit :: ExitCode -> IO ()
checkExit ExitSuccess = pure ()
checkExit n           = exitWith n

main :: IO ()
main = hspec .
  describe "Simple parsing test" $ do
    it "should not work for errored files" $ do
      let p = parseNaiveSimple
      p "" `shouldSatisfy` isLeft
      p "module" `shouldSatisfy` isLeft
      p "where" `shouldSatisfy` isLeft
    it "should parse module name" $ do
      let p s = moduleNameList . topLevelModuleName <$> parseNaiveSimple s
      p "module A where" `shouldBe` Right [T.pack "A"]
      p "module A.B.C where" `shouldBe` Right (T.pack <$> ["A", "B", "C"])
