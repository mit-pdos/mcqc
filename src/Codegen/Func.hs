{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, TemplateHaskell, RecordWildCards  #-}
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
    CFunc { _fname :: Text, _templtypes :: [Text], _ftype :: Text, _fargs :: [CDef], _fbody :: CExpr }
    | CFuncEmpty {}
  deriving (Eq, Generic, Semigroup, ToJSON)

instance Monoid CFunc where
  mempty = CFuncEmpty {}
  -- XXX: Implement fusion here with mappend
  mappend a b = a

makeLenses ''CFunc

-- Declarations to C Function
toCDecl :: Declaration -> CFunc
toCDecl FixDecl  { .. } = toCFunc (head fixlist)
-- XXX: Implement other declarations
toCDecl IndDecl  { .. } = mempty
toCDecl TypeDecl { .. } = mempty
toCDecl TermDecl { .. } = mempty

-- Nat -> Nat -> Bool ==> [Nat, Nat, Bool]
getCTypeList :: Typ -> [Text]
getCTypeList TypArrow { left = lt, right = rt } = (getCTypeList lt) ++ (getCTypeList rt)
getCTypeList TypVar { name = n, args = al } = ["<getCTypeLast PLACEHOLDER>"]
getCTypeList TypGlob { name = n } = [toCType n]

-- Get the argument names from lamda definition
getCNames :: Expr -> [Text]
getCNames ExprLambda { argnames = al } = map toCName al
getCNames ExprRel {..} = [toCName name]
getCNames ExprGlobal {..} = [toCName name]

-- Hacky and bad in many ways
addTemplates :: [Text] -> [Text]
addTemplates incls
    | "List<T>" `elem` incls || "Optional<T>" `elem` incls = [ "T" ]
    | otherwise = []

-- Fixpoint declaration to C Function
toCFunc :: Fix -> CFunc
toCFunc Fix { name = Just n, typ = t, value = l@ExprLambda {..} } = CFunc n templateTypes retType refArguments cbody
    where arguments = getCDefExtrap args argTypes
          templateTypes = addTemplates (getCTypeList t)
          refArguments = map addRef arguments
          cbody = translateCNames $ semantics $ toCExpr body
          args = getCNames l
          argTypes = init . getCTypeList $ t
          retType = last . getCTypeList $ t
toCFunc Fix { name = Nothing, .. } = error "Anonymous Fixpoints not supported"


