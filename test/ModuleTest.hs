module ModuleTest where
import Test.Hspec
import Schema

testModuleWhat :: Either String Module -> IO ()
testModuleWhat (Left s) = s `shouldBe` "Bad module 'what' key bad_identifier"
testModuleWhat (Right m) = what (m :: Module) `shouldBe` "module"

