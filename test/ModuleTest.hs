{-# LANGUAGE DuplicateRecordFields, TemplateHaskell #-}
module ModuleTest where
import Test.Hspec
import Codegen.Schema (Module)

testModuleWhat :: Either String Module -> IO ()
testModuleWhat (Left s) = putStrLn s
testModuleWhat (Right m) = what (m :: Module) `shouldBe` "module"

