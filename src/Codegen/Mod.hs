{-# LANGUAGE DuplicateRecordFields, TemplateHaskell #-}
module Codegen.Mod where
import Codegen.Decl (makeDecl)
import Codegen.Schema
import Data.ByteString.Lazy.Char8 (ByteString)

-- Module high level function
makeModule :: Module -> Either String ByteString
makeModule Module { what = "module", used_modules = Nothing, declarations = decls } = mconcat <$> mapM makeDecl decls
makeModule Module { what = "module", used_modules = Just ml, declarations = decls } = mconcat <$> mapM makeDecl decls -- TODO: Implement linking
makeModule Module { what = s } = Left $ "Bad module 'what' key " ++ s

