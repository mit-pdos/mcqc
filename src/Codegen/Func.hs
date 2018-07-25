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
  deriving (Eq, Generic, ToJSON)

-- XXX: Implement fusion here with mappend
instance Semigroup CFunc where
  (<>) a b = a
instance Monoid CFunc where
  mempty = CFuncEmpty {}
  mappend = (<>)

-- Generate Lens code
makeLenses ''CFunc

-- Declarations to C Function
toCDecl :: Declaration -> CFunc
toCDecl FixDecl  { .. } = toCFunc (head fixlist)
toCDecl TermDecl { .. } = CFunc name templateTypes retType arguments cbody
    where templateTypes = addTemplates (getCTypeList typ)
          cbody = translateCNames $ semantics $ toCExpr val
          retType = last . getCTypeList $ typ
          argTypes = init . getCTypeList $ typ
          arguments = getCDefExtrap (getNames val) argTypes

-- XXX: Implement other declarations
toCDecl IndDecl  { .. } = mempty
toCDecl TypeDecl { .. } = mempty

-- Nat -> Nat -> Bool ==> [Nat, Nat, Bool]
getCTypeList :: Typ -> [Text]
getCTypeList TypArrow { left = lt, right = rt } = (getCTypeList lt) ++ (getCTypeList rt)
getCTypeList TypVar   { name = n, args = al } = ["<getCTypeLast PLACEHOLDER>"]
getCTypeList TypGlob  { name = n } = [toCType n]

-- Hacky and bad in many ways
addTemplates :: [Text] -> [Text]
addTemplates incls
    | "List<T>" `elem` incls || "Optional<T>" `elem` incls = [ "T" ]
    | otherwise = []

-- Fixpoint declaration to C Function
toCFunc :: Fix -> CFunc
toCFunc Fix { name = Just n, value = ExprLambda { .. }, .. } = CFunc n templateTypes retType refArguments cbody
    where arguments = getCDefExtrap argnames argTypes
          templateTypes = addTemplates $ getCTypeList ftyp
          refArguments = map addRef arguments
          cbody = translateCNames . semantics $ toCExpr body
          argTypes = init . getCTypeList $ ftyp
          retType = last . getCTypeList $ ftyp
toCFunc Fix { name = Just n, value = l } = error "Fixpoint not followed by an ExprLambda in undefined behavior"
toCFunc Fix { name = Nothing, .. } = error "Anonymous Fixpoints are undefined behavior"

