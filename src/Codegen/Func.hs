{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, TypeSynonymInstances, RecordWildCards, FlexibleInstances  #-}
module Codegen.Func (CFunc, toCFunc) where
import Codegen.Rewrite (toCType)
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

-- Utility function
-- Get the next string lexicographically
incrementString :: String -> String
incrementString []          = ['a']
incrementString ('z':xs)    = 'a' : incrementString xs
incrementString (x:xs)      = succ x : xs

-- Define a succ for Strings
instance Enum [Char] where
  succ = reverse . incrementString . reverse

-- Nat -> Nat -> Bool ==> [Nat, Nat, Bool]
getCTypeList :: Typ -> [String]
getCTypeList TypArrow { left = lt, right = rt } = (getCTypeList lt) ++ (getCTypeList rt)
getCTypeList TypVar { name = n, args = Nothing } = ["<getCTypeList PLACEHOLDER>"]
getCTypeList TypVar { name = n, args = Just al } = ["<getCTypeLast PLACEHOLDER>"]
getCTypeList TypGlob { name = n } = [toCType n]

-- Nat -> Nat -> Bool ==> Bool
getCRetType :: Typ -> String
getCRetType = last . getCTypeList

-- Get the argument names from lamda definition
getCLambdaNames :: Expr -> [String]
getCLambdaNames ExprLambda { argnames = al } = al

-- If there are less named arguments that positional arguments in the type signature, extrapolate
-- and if clang gives an "Unused argument warning" then ok
getCDefExtrap :: [String] -> [String] -> [CDef]
getCDefExtrap [] [] = []
getCDefExtrap [x] [y] = [CDef x y]
getCDefExtrap [x] (y:ys) = (CDef x y):(getCDefExtrap [succ x] ys)
getCDefExtrap (x:xs) [y] = (CDef x y):(getCDefExtrap xs [succ y])
getCDefExtrap (x:xs) (y:ys) = (CDef x y):(getCDefExtrap xs ys)

-- Fixpoint declaration to C Function
toCFunc :: Fix -> CFunc
toCFunc Fix { name = Just n, typ = t, value = ExprLambda {..} } = CFunc n (getCRetType t) defs (toCExpr body)
    where defs = getCDefExtrap argnames argtypes
            where
                argtypes = (init . getCTypeList) t
toCFunc Fix { name = Nothing, typ = t, value = ExprLambda {..} } = CFunc "NoNamePlaceholder" (getCRetType t) defs (toCExpr body)
    where defs = getCDefExtrap argnames argtypes
            where
                argtypes = (init . getCTypeList) t


