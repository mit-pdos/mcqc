{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, TypeSynonymInstances, RecordWildCards, FlexibleInstances  #-}
module Codegen.Func (CFunc, toCFunc) where
import Codegen.Rewrite (toCType, toCName)
import Codegen.Expr
import Codegen.Defs
import Codegen.Utils
import Parser.Decl
import Parser.Fix
import Parser.Expr
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

-- C function definition, ie: int main(int argc, char **argv)
data CFunc = CFunc { fname :: Text, ftype :: Text, fargs :: [CDef], fbody :: CExpr } -- TODO: Text as a PLACEHOLDER
  deriving (Eq)

instance Pretty CFunc where
  pretty f = (pretty . ftype) f <+> mkFuncSig (fname f) (map pretty $ fargs f)
                    <> vcat ["{", (tab . pretty . fbody) f, "}"] <> line

-- Nat -> Nat -> Bool ==> [Nat, Nat, Bool]
getCTypeList :: Typ -> [Text]
getCTypeList TypArrow { left = lt, right = rt } = (getCTypeList lt) ++ (getCTypeList rt)
getCTypeList TypVar { name = n, args = al } = ["<getCTypeLast PLACEHOLDER>"]
getCTypeList TypGlob { name = n } = [toCType n]

-- Nat -> Nat -> Bool ==> Bool
getCRetType :: Typ -> Text
getCRetType = last . getCTypeList

-- Get the argument names from lamda definition
getCNames :: Expr -> [Text]
getCNames ExprLambda { argnames = al } = map toCName al
getCNames ExprRel {..} = [toCName name]
getCNames ExprGlobal {..} = [toCName name]

-- Fixpoint declaration to C Function
toCFunc :: Fix -> CFunc
toCFunc Fix { name = Just n, typ = t, value = l@ExprLambda {..} } = CFunc n (getCRetType t) defs (toCExpr body)
    where defs = getCDefExtrap args argtypes
            where
                args = getCNames l
                argtypes = (init . getCTypeList) t
toCFunc Fix { name = Nothing, typ = t, value = l@ExprLambda {..} } = CFunc "NoNamePlaceholder" (getCRetType t) defs (toCExpr body)
    where defs = getCDefExtrap args argtypes
            where
                args = getCNames l
                argtypes = (init . getCTypeList) t


