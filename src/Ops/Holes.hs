{-# language OverloadedStrings #-}
module Ops.Holes where
import Prelude hiding (putStr, getLine)
import Codegen.Rewrite
import Data.Text (Text)
import Data.Text.IO
import qualified Data.Text as T

-- Read type aliases from stdin
fillHole :: Text -> IO Text
fillHole name = do
    putStr $ T.concat ["Hole found `", name, "`:= "]
    t <- getLine
    -- Test if valid CType by how it reacts with toCType
    if (t /= (toCTBase t)) then return (toCTBase t)
    else fillHole name
