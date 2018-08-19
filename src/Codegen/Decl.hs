{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, OverloadedStrings, TemplateHaskell, RecordWildCards  #-}
module Codegen.Decl where
import GHC.Generics
import Codegen.Rewrite
import Codegen.Expr
import Codegen.Defs
import Common.Utils
import Parser.Decl
import Parser.Fix
import Parser.Expr
import Ops.Holes
import Control.Lens
import Data.Aeson
import Data.Text (Text)
import System.IO.Unsafe
import qualified Data.Text as T

-- Global scope C definitions, 
-- TODO: Records, Types, global vars
data CDecl =
    CFunc { _fname :: Text, _templtypes :: [Text], _ftype :: Text, _fargs :: [CDef], _fbody :: CExpr }
    | CAlias { _tname :: Text, _tvalue :: CExpr }
    | CEmpty {}
  deriving (Eq, Generic, ToJSON)

-- Generate Lens code
makeLenses ''CDecl

-- Nat -> Nat -> Bool ==> [Nat, Nat, Bool]
getCTypeList :: Typ -> [Text]
getCTypeList TypArrow   { .. }                          = (getCTypeList left) ++ (getCTypeList right)
getCTypeList TypVar     { name = n, .. }                = [toCType n]
getCTypeList TypGlob    { name = n, targs = [] }        = [toCType n]
getCTypeList TypGlob    { name = "Datatypes.list", .. } = [toCType "Datatypes.list"]
getCTypeList TypGlob    { name = n, .. }                = concat $ map getCTypeList targs
getCTypeList TypVaridx  { .. }                          = error "Generic indexed types not supported, undefined behavior"
getCTypeList TypDummy   {}                              = ["void"]
getCTypeList TypUnknown {}                              = ["void"]

-- Declarations to C Function
toCDecl :: Declaration -> CDecl
-- Define fixpoint by translating it to a single return-match statement
toCDecl FixDecl  { fixlist = [ Fix { name = Just n, value = ExprLambda { .. }, .. } ] } = 
    CFunc n templateTypes retType arguments cbody
    where cbody = CExprCall "return" [toCExpr body]
          arguments = getCDefExtrap argnames argTypes
          templateTypes = addTemplates $ getCTypeList ftyp
          retType = last . getCTypeList $ ftyp
          argTypes = init . getCTypeList $ ftyp
-- Define imperative function by unrolling the proc monad to sequential statement expressions
toCDecl TermDecl { val = ExprLambda { .. }, .. } = 
    CFunc name templateTypes retType arguments (toCExpr body)
    where arguments = getCDefExtrap argnames argTypes
          templateTypes = addTemplates (getCTypeList typ)
          retType = last . getCTypeList $ typ
          argTypes = init . getCTypeList $ typ
-- Define type aliases, fill holes from user
toCDecl TypeDecl { tval = TypUnknown {}, .. } = CAlias name typexpr 
    where typexpr = CExprVar . unsafePerformIO . fillHole $ name
toCDecl d@TypeDecl { .. } = error $ "Not implemented: " ++ (show d)
-- Sanitize declarations for correctness
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = l } ] } = error "Fixpoint not followed by an ExprLambda is undefined behavior"
toCDecl FixDecl { fixlist = [ Fix { name = Nothing, .. } ] }       = error "Anonymous Fixpoints are undefined behavior"
toCDecl FixDecl { fixlist = [] }                                   = error "Empty fixlist for declaration found, undefined behavior"
toCDecl FixDecl { fixlist = f:fl }                                 = error "Fixlist with multiple fixpoints is undefined behavior"
-- XXX: Implement other declarations
toCDecl IndDecl  { .. } = CEmpty {}

