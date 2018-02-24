module Main where
import Prelude hiding (readFile)
import Test.Hspec
import Data.ByteString.Lazy.Char8 (readFile)
import ModuleTest (testModuleWhat)
import Codegen

main :: IO ()
main = hspec $ do
  describe "JSON parseModule tests" $ do
    it "Parses a JSON file to a Module" $
      readFile "test/JSON/bt.json" >>= testModuleWhat . parse

