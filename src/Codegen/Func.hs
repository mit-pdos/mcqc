{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, OverloadedStrings, TemplateHaskell, RecordWildCards  #-}
module Codegen.Func where
import GHC.Generics
import Codegen.Rewrite
import Codegen.Expr
import Codegen.Defs
import Codegen.Utils
import Parser.Decl
import Parser.Fix
import Parser.Expr
import Sema.Pipeline
import Control.Lens
import Data.Monoid
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

-- C function definition, ie: int main(int argc, char **argv)
data CFunc =
    CFuncFix { _fname :: Text, _templtypes :: [Text], _ftype :: Text, _fargs :: [CDef], _fbody :: CExpr }
    | CFuncImp { _fname :: Text, _templtypes :: [Text], _ftype :: Text, _fargs :: [CDef], _fstmt :: CExpr }
    | CFuncEmpty {}
  deriving (Eq, Generic, ToJSON)

-- XXX: Implement fusion here with mappend
instance Semigroup CFunc where
  (<>) a b = a
instance Monoid CFunc where
  mempty = CFuncEmpty {}
  mappend = (<>)

-- Generate Lens code
makeLenses ''CFunc

-- Nat -> Nat -> Bool ==> [Nat, Nat, Bool]
getCTypeList :: Typ -> [Text]
getCTypeList TypArrow { left = lt, right = rt }       = (getCTypeList lt) ++ (getCTypeList rt)
getCTypeList TypVar   { name = n, .. }                = [toCType n]
getCTypeList TypGlob  { name = n, targs = [] }        = [toCType n]
getCTypeList TypGlob  { name = "Datatypes.list", .. } = [toCType "Datatypes.list"]
getCTypeList TypGlob  { name = n, .. }                = concat $ map getCTypeList targs
getCTypeList TypVaridx  { .. }                        = error "Generic indexed types not supported, undefined behavior"
getCTypeList TypDummy   {}                            = [ "void" ]
getCTypeList TypUnknown {}                            = [ "void" ]

-- Declarations to C Function
toCDecl :: Declaration -> CFunc
-- Define fixpoint by translating it to a single return-match statement
toCDecl FixDecl  { fixlist = [ Fix { name = Just n, value = ExprLambda { .. }, .. } ] } = CFuncFix n templateTypes retType refArguments cbody
    where arguments = getCDefExtrap argnames argTypes
          templateTypes = addTemplates $ getCTypeList ftyp
          refArguments = map addRef arguments
          cbody = translateCNames . semantics $ toCExpr body
          argTypes = init . getCTypeList $ ftyp
          retType = last . getCTypeList $ ftyp
-- Define imperative function by unrolling the proc monad to sequential statement expressions
toCDecl TermDecl { val = ExprLambda { .. }, .. } = CFuncImp name templateTypes retType arguments cbody
    where templateTypes = addTemplates (getCTypeList typ)
          cbody = translateCNames $ semantics $ toCExpr body
          retType = last . getCTypeList $ typ
          argTypes = init . getCTypeList $ typ
          arguments = getCDefExtrap argnames argTypes
-- Sanitize declarations for correctness
toCDecl FixDecl { fixlist = [ Fix { name = Just n, value = l } ] } = error "Fixpoint not followed by an ExprLambda is undefined behavior"
toCDecl FixDecl { fixlist = [ Fix { name = Nothing, .. } ] }       = error "Anonymous Fixpoints are undefined behavior"
toCDecl FixDecl { fixlist = [] }                                   = error "Empty fixlist for declaration found, undefined behavior"
toCDecl FixDecl { fixlist = f:fl }                                 = error "Fixlist with multiple fixpoints is undefined behavior"
-- XXX: Implement other declarations
toCDecl IndDecl  { .. } = mempty
toCDecl TypeDecl { .. } = mempty


