{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, OverloadedStrings, TemplateHaskell, RecordWildCards  #-}
module Codegen.Decl where
import GHC.Generics
import Codegen.Rewrite
import Codegen.Expr
import Parser.Decl
import Parser.Fix
import Parser.Expr
import Control.Lens
import Data.Aeson
import Data.Text (Text)
import System.IO.Unsafe
import Debug.Trace
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B

-- Global scope C definitions, 
-- TODO: Records, Types, global vars
data CDecl =
    CDFunc { _fn :: Text, _ftype :: CType, _fargs :: [Text], _fbody :: CExpr }
    | CDType { _tname :: Text, _tval :: CType }
    | CDExpr { _ename :: Text, _expr :: CExpr }
    | CDEmpty { }
  deriving (Eq, Generic, ToJSON)

-- Generate Lens code
makeLenses ''CDecl

instance Show CDecl where
    show = B.unpack . encodePretty

-- Declarations to C Function
toCDecl :: Declaration -> CDecl
-- toCDecl d | trace ("=== DEBUG " ++ show d) False = undefined
-- Define fixpoint by translating it to a single return-match statement
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = ExprLambda { .. }, .. } ] } = 
    CDFunc n funcT argnames cbody
    where cbody = CExprCall "return" [toCExpr body]
          funcT = toCType ftyp
-- Define imperative function by unrolling the proc monad to sequential statement expressions
toCDecl TermDecl { val = ExprLambda { .. }, .. } = 
    CDFunc name funcT argnames cbody
    where cbody = toCExpr body
          funcT = toCType typ
-- Define type aliases, fill holes from user
toCDecl TypeDecl { .. } = CDType (toCTBase name) $ toCType tval
-- Sanitize declarations for correctness
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = l } ] } = error "Fixpoint not followed by an ExprLambda is undefined behavior"
toCDecl FixDecl { fixlist = [ Fix { name = Nothing, .. } ] }       = error "Anonymous Fixpoints are undefined behavior"
toCDecl FixDecl { fixlist = [] }                                   = error "Empty fixlist for declaration found, undefined behavior"
toCDecl FixDecl { fixlist = f:fl }                                 = error "Fixlist with multiple fixpoints is undefined behavior"
-- XXX: Implement other declarations
toCDecl IndDecl  { .. } = CDEmpty {}

