{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, TypeSynonymInstances, RecordWildCards, FlexibleInstances  #-}
module Codegen.Func (CFunc, toCFunc) where
import Codegen.Rewrite (toCType, toCName)
import Codegen.Expr
import Codegen.Defs
import Parser.Decl
import Parser.Fix
import Parser.Expr

-- C function definition, ie: int main(int argc, char **argv)
data CFunc = CFunc { fname :: String, ftype :: String, fargs :: [CDef], fbody :: CExpr } -- TODO: String as a PLACEHOLDER
  deriving (Eq)

instance Show CFunc where
  show f = concat [ftype f, " ", fname f," (", show (fargs f), ") ", "{\n", "\t" ++ (show $ fbody f) ++ "\n",  "}\n"]

-- Nat -> Nat -> Bool ==> [Nat, Nat, Bool]
getCTypeList :: Typ -> [String]
getCTypeList TypArrow { left = lt, right = rt } = (getCTypeList lt) ++ (getCTypeList rt)
getCTypeList TypVar { name = n, args = al } = ["<getCTypeLast PLACEHOLDER>"]
getCTypeList TypGlob { name = n } = [toCType n]

-- Nat -> Nat -> Bool ==> Bool
getCRetType :: Typ -> String
getCRetType = last . getCTypeList

-- Get the argument names from lamda definition
getCNames :: Expr -> [String]
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


