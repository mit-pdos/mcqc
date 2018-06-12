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
foreign import ccall "clangToJSON" c_clangToJSON :: CString -> CString

-- Top level namespace
data Namespace = Namespace { namespace :: Text, functions :: [ FuncSig ] }
    deriving (Show, Eq, Generic, FromJSON)

-- Function Signature
data FuncSig = FuncSig { name :: Text, typ :: Text, args :: [ Text ] }
    deriving (Show, Eq, Generic, FromJSON)

parseHpp :: String -> Either String Namespace
parseHpp fn =
    let inputStr = unsafePerformIO $ newCAString fn in
        let jsonStr = c_clangToJSON inputStr in
            parseJson jsonStr
                where parseJson = eitherDecode . B.pack . unsafePerformIO . peekCAString

