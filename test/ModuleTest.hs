module ModuleTest where
import Test.Hspec
import Schema

testModuleWhat :: Either String Module -> IO ()
testModuleWhat (Left s) = s `shouldBe` "Bad identifier, failing..."
testModuleWhat (Right m) = what (m :: Module) `shouldBe` "module"

