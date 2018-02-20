import Prelude hiding (readFile)
import Test.Hspec
import Schema
import Data.ByteString.Lazy.Char8 (readFile)
import ModuleTest (testModuleWhat)

main :: IO ()
main = hspec $ do
  describe "JSON parseModule tests" $ do
    it "Parses a JSON file to a Module" $
      parseModule <$> readFile "test/JSON/bt.json" >>= testModuleWhat

    it "Parses a bad JSON file and should throw an error" $
      parseModule <$> readFile "test/JSON/bad.json" >>= testModuleWhat

