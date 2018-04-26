{-# LANGUAGE DuplicateRecordFields, TemplateHaskell #-}
module ModuleTest where
import Test.Hspec
import Parser.Mod

testModuleWhat :: Either String Module -> IO ()
testModuleWhat (Left s) = putStrLn s
testModuleWhat (Right m) = print m

