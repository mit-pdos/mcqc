{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards, DeriveAnyClass #-}
module Clang.CParser where
import GHC.Generics hiding (Constructor)
import Data.Aeson
import Prelude hiding (lookup)
import Foreign
import Foreign.C
import Foreign.C.String
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO.Unsafe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

-- FFI libclang
foreign import ccall "clangToJSON" c_clangToJSON :: CString -> CString

-- Top level namespace
data Namespace = Namespace { namespace :: Text, functions :: [ FuncSig ] }
    deriving (Show, Eq, Generic, FromJSON)

-- Function Signature
data FuncSig = FuncSig { name :: Text, typ :: Text, args :: [ Text ] }
    deriving (Show, Eq, Generic, FromJSON)

instance Pretty Namespace where
    pretty Namespace { .. }  = "== "
                               <+> pretty namespace
                               <+> line
                               <+> nest 4 (vcat (map pretty functions))
instance Pretty FuncSig where
    pretty FuncSig { .. } = pretty typ
                            <+> pretty name
                            <+> "("
                            <> vcat (map (\s -> pretty s <> ", ") (init args))
                            <> pretty (last args)
                            <> ")"

-- Calls into cbits/clangjson.cpp
parseHpp :: String -> IO Namespace
parseHpp fn = do
    inputStr <- newCAString fn
    jsonStr <- peekCAString (c_clangToJSON inputStr) -- Call to FFI libclang implementation
    return $ (unwrapEither . eitherDecode . B.pack) jsonStr
        where unwrapEither (Right s) = s
              unwrapEither (Left err) = error err

