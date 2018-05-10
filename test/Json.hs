module Main where
import Prelude hiding (readFile)
import Test.Hspec
import Data.Aeson
import Data.ByteString.Lazy.Char8
import ModuleTest (testModuleWhat)
import Parser.Mod

-- Parse JSON file into a Module
parse :: ByteString -> Either String Module
parse buffer = eitherDecode buffer :: Either String Module

main :: IO ()
main = hspec $ do
  describe "JSON parseModule tests" $ do
    it "Parses a JSON file to a Module" $
      readFile "test/JSON/bt.json" >>= testModuleWhat . parse

