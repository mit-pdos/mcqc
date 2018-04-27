{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, TypeSynonymInstances, FlexibleInstances  #-}
module Codegen.Func (CDef, CFunc, toCFunc) where
import Parser.Decl
import Parser.Fix
import Parser.Expr

-- C typed definition, ie: "int foo"
data CDef = CDef { cname :: String, ctype :: String }
  deriving (Eq)

-- C function definition, ie: int main(int argc, char **argv)
data CFunc = CFunc { fname :: String, ftype :: String, fargs :: [CDef], fbody :: String } -- TODO: String as a PLACEHOLDER
  deriving (Eq)

-- Pretty print C++ from these types
instance Show CDef where
  show d = (ctype d) ++ " " ++ (cname d)

-- Overlaps with Show [a]
instance {-# OVERLAPPING #-} Show [CDef] where
  show [] = ""
  show [d] = show d
  show (d:dl) = (show d) ++ ", " ++ (show dl)

instance Show CFunc where
  show f = concat [ftype f, " ", fname f," (", show (fargs f), ") ", "{\n", "\t" ++ (fbody f) ++ "\n",  "}\n"]

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
getCTypeList TypGlob { name = n } = [n]

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
getCDefExtrap [x] (y:ys) = (CDef x y):(getCDefExtrap [succ x] ys)
getCDefExtrap (x:xs) [y] = (CDef x y):(getCDefExtrap xs [succ y])
getCDefExtrap (x:xs) (y:ys) = (CDef x y):(getCDefExtrap xs ys)

-- Fixpoint declaration to C Function
toCFunc :: Fix -> CFunc
toCFunc Fix { name = Just n, typ = t, value = v} = CFunc n (getCRetType t) defs "<PLACEHOLDER>"
    where defs = getCDefExtrap argnames argtypes
            where
                argnames = getCLambdaNames v
                argtypes = (init . getCTypeList) t
toCFunc Fix { name = Nothing, typ = t, value = v} = CFunc "noname" (getCRetType t) defs "<PLACEHOLDER>"
    where defs = getCDefExtrap argnames argtypes
            where
                argnames = getCLambdaNames v
                argtypes = (init . getCTypeList) t


