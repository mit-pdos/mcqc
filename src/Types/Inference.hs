{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Types.Inference where
import CIR.Expr
import CIR.Decl
import Common.Flatten
import Codegen.Rewrite
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Common.Config as Conf
import Control.Lens
import Debug.Trace

-- Unify the second type with the first type
unify :: CType -> CType -> CType
unify CTFunc { _fret = a, _fins = c} CTFunc { _fret = b, _fins = d}
    | length c == length d = CTFunc (unify a b) $ zipWith unify c d
    | otherwise = error $ "Attempting to unify func types with different num of args" ++ show c ++ " " ++ show d
unify CTExpr { _tbase = CTBase { _base = b1 }, _tins = a } CTExpr { _tbase = CTBase { _base = b2 }, _tins = b }
    | length a == length b = CTExpr (CTBase b1) $ zipWith unify a b
    | b1 /= b2  = error $ "Cannot unify different base types " ++ show b1 ++ " " ++ show b2
    | otherwise = error $ "Attempting to unify list types with different num of args" ++ show a ++ " " ++ show b
unify t CTUndef {} = t
unify a b = error $ "Unsure how to unify " ++ show a ++ " " ++ show b

-- TODO: This is buggy and not very smart, does not recurse into lists options etc.
-- Got to find a better way to unify a given return type with untyped rvalues
-- Maximally infer return types
maxinsert :: CType -> CExpr -> CExpr
-- maxinsert a b | trace ("DBG for type " ++ show a ++ " expr " ++ show b) False = undefined
maxinsert t CExprCall { .. }
    -- Return preserves the type
    | _fname == "return" = CExprCall "return" $ map (maxinsert t) _fparams
    -- A match preserves the type if the lambdas return it (omit matched object)
    | _fname == "match"  = CExprCall "match" $ head _fparams:map (maxinsert t) (tail _fparams)
    -- Function call obfuscate the return type, ignore them
    | otherwise          = CExprCall _fname _fparams
-- Return is implicit in lambda bodies
maxinsert CTExpr { _tbase = CTBase { _base = "list" }, _tins = [t] } CExprLambda { _lbody = CExprList { .. }, .. } =
    CExprLambda _largs typedlist
    where typedlist = CExprList unified _elems
          unified   = unify t _etype
maxinsert CTExpr { _tbase = CTBase { _base = "option" }, _tins = [t] } CExprLambda { _lbody = CExprOption { .. }, .. } =
    CExprLambda _largs typedoption
    where typedoption = CExprOption unified _val
          unified   = unify t _otype
-- Or explicit if it comes from the first rule handling return calls
maxinsert CTExpr { _tbase = CTBase { _base = "list" }, _tins = [t] } CExprList { .. } =
    CExprList unified _elems
    where unified = unify t _etype
maxinsert CTExpr { _tbase = CTBase { _base = "option" }, _tins = [t] } CExprOption { .. } =
    CExprOption unified _val
    where unified = unify t _otype
maxinsert t o = descend (maxinsert t) o

