module Main where

-- import Test.Hspec

import           OwO.Syntax.Parser
import           Prelude           hiding (lex)

main :: IO ()
main = print . lex $
 "module data + data\n" ++
 "a = b"
