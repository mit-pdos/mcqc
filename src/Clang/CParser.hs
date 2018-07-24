{-# LANGUAGE OverloadedStrings  #-}
module Clang.CParser where
import Clang.Namespaces
import Data.Aeson
import Foreign.C
import Foreign.C.String
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T

-- FFI libclang
-- foreign import ccall "clangToJSON" c_clangToJSON :: CString -> CString

-- Calls into cbits/clangjson.cpp
-- parseHpp :: String -> IO Namespace
-- parseHpp fn = do
--     inputStr <- newCAString fn
--     jsonStr <- peekCAString (c_clangToJSON inputStr) -- Call to FFI libclang implementation
--     return $ (unwrapEither . eitherDecode . B.pack) jsonStr
--         where unwrapEither (Right s) = s
--               unwrapEither (Left err) = error err

-- isLib :: FilePath -> Bool
-- isLib s = T.takeEnd 4 (T.pack s) == ".hpp"
