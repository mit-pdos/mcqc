{-# LANGUAGE DeriveGeneric, ForeignFunctionInterface, DeriveAnyClass #-}
module Clang.CParser where
import GHC.Generics hiding (Constructor)
import Data.Aeson
import Data.Text
import Prelude hiding (lookup)
import Foreign
import Foreign.C
import Foreign.C.String
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO.Unsafe

-- FFI libclang
foreign import ccall "clangToJSON" c_clangToJSON :: CString

-- Top level namespace
data Namespace = Namespace { namespace :: Text, functions :: [ FuncSig ] }
    deriving (Show, Eq, Generic, FromJSON)

-- Function Signature
data FuncSig = FuncSig { name :: Text, typ :: Text, args :: [ Text ] }
    deriving (Show, Eq, Generic, FromJSON)

parseHpp :: CString -> Maybe Namespace
parseHpp = decode . B.pack . unsafePerformIO . peekCAString

hppTree :: Maybe Namespace
hppTree = parseHpp c_clangToJSON
