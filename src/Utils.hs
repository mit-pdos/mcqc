module Utils where
import Prelude hiding (concat)
import Data.Char (toLower, toUpper)
import Data.ByteString.Lazy.Char8 (ByteString, concat, pack, unpack)

debugToBytestring :: Either String ByteString -> ByteString
debugToBytestring (Right bs) = concat $ map pack [" {", unpack bs, "} "]
debugToBytestring (Left s) = concat $ map pack ["! ", s, " !"]

capitalised [] = []
capitalised x = toUpper (head x) : map toLower (tail x)

lowercase :: [Char] -> [Char]
lowercase [] = []
lowercase (h:ts) = (toLower h):(lowercase ts)
