{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Types.Inference where
import CIR.Expr
import CIR.Decl
import Codegen.Rewrite
import Common.Utils
import Data.List (nub)
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.MonoTraversable
import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Common.Config as Conf

type Context a = Map Text [a]

merge :: Context CType -> Context CType -> Context CType
merge = M.unionWith (zipWith unify)

-- Unify the second type with the first type
unify :: CType -> CType -> CType
-- Any type is better than undefined type
unify t CTUndef {} = t
unify CTUndef {} t = t
unify CTFunc { _fret = a, _fins = ina} CTFunc { _fret = b, _fins = inb}
    | length ina == length inb = CTFunc (unify a b) $ zipWith unify ina inb
    | otherwise = error $ "Attempting to unify func types with different args" ++ show ina ++ " " ++ show inb
-- Ignore Proc monad wrapped types
unify CTExpr { _tbase = CTBase { _base = "proc" }, _tins = [a] } t = unify a t
unify t CTExpr { _tbase = CTBase { _base = "proc" }, _tins = [a] } = unify t a
unify CTExpr { _tbase = a , _tins = ina } CTExpr { _tbase = b , _tins = inb }
    | length ina == length inb = CTExpr (unify a b) $ zipWith unify ina inb
    | otherwise = error $ "Attempting to unify list types with different args" ++ show ina ++ " " ++ show inb
unify CTBase { _base = a } CTBase { _base = b }
    | a == b  = CTBase a
    | otherwise = error $ "Cannot unify different base types " ++ show a ++ " " ++ show b
unify CTFree { _idx = a } CTFree { _idx = b }
    | a == b = CTFree a
    | otherwise = error $ "Cannot unify different indices " ++ show a ++ " " ++ show b
-- Unify free parameters, here we're assuming Coq has already type-checked this
unify CTFree { .. } t = t
unify t CTFree { .. } = t
unify a b = error $ "Unsure how to unify " ++ show a ++ " " ++ show b

-- TODO: This is not very smart, does not recurse into lists options etc.
-- Maximally insert return types
maxinsert :: CType -> CExpr -> CExpr
-- Unwrap proc types to contained type
maxinsert t CExprCall { .. }
    -- Return preserves the type
    | _fname == "return" = CExprCall "return" $ map (maxinsert t) _fparams
    -- A match preserves the type if the lambdas return it (omit matched object)
    | _fname == "match"  = CExprCall "match" $ head _fparams:map (maxinsert t) (tail _fparams)
    -- An app also preserves the types of its arguments into the return type
    | _fname == "app"    = CExprCall "app" $ map (maxinsert t) _fparams
    -- Function call obfuscate the return type, ignore them
    | otherwise          = CExprCall _fname _fparams
-- Or explicit if it comes from the first rule handling return calls
maxinsert CTExpr { _tbase = CTBase { _base = "list" }, _tins = [t] } CExprList { .. } =
    CExprList unified _elems
    where unified = unify t _etype
maxinsert CTExpr { _tbase = CTBase { _base = "option" }, _tins = [t] } CExprOption { .. } =
    CExprOption unified _val
    where unified = unify t _otype
maxinsert t s@CExprSeq { .. } = listToSeq $ first ++ [retexpr]
    where retexpr = maxinsert t . last . seqToList $ s
          first   = init . seqToList $ s
maxinsert t o = omap (maxinsert t) o

