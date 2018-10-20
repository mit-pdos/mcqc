{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Classparser.Parser where
import Classparser.Regex
import CIR.Expr
import Types.Inference
import Data.Text (Text)
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Text         as T
import qualified Data.List         as L
import qualified Data.Map.Strict   as M

-- Parse a typeclass .v definition into a Context of type signatures
parser :: String -> Context CType
parser body = M.map (map mktype) . lexer $ body
    where binders = snd . getclass $ body
          -- Search for binder substitution
          mktype txttype =
              case L.elemIndex txttype binders of
                  -- Found a binder, substitute with free var
                  (Just n)  -> CTFree n
                  -- Found a bound type, either Expr or Base
                  (Nothing) ->
                      case map T.pack . splitOn " " . T.unpack $ txttype of
                          ([base])  -> CTBase base  -- Base type
                          (base:ts) -> CTExpr (CTBase base) $ map mktype ts  -- Composite type

