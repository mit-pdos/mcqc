{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Types.Inference where
import CIR.Expr
import CIR.Decl
import Codegen.Rewrite
import Common.Utils
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Common.Config as Conf
import Data.MonoTraversable
import Control.Lens
import Debug.Trace

-- Unify the second type with the first type
unify :: CType -> CType -> CType
unify CTFunc { _fret = a, _fins = c} CTFunc { _fret = b, _fins = d}
    | length c == length d = CTFunc (unify a b) $ zipWith unify c d
    | otherwise = error $ "Attempting to unify func types with different num of args" ++ show c ++ " " ++ show d
unify CTExpr { _tbase = CTBase { _base = b1 }, _tins = a } CTExpr { _tbase = CTBase { _base = b2 }, _tins = b }
    | length a == length b && b1 == b2 = CTExpr (CTBase b1) $ zipWith unify a b
    | b1 /= b2  = error $ "Error: Cannot unify different base types " ++ show b1 ++ " " ++ show b2
    | otherwise = error $ "Error: Attempting to unify list types with different num of args" ++ show a ++ " " ++ show b
unify CTExpr { _tbase = CTBase { _base = "proc" }, _tins = [a] } t = unify a t
unify t CTUndef {} = t
unify a b = error $ "Error: Unsure how to unify " ++ show a ++ " " ++ show b

-- TODO: This is not very smart, does not recurse into lists options etc.
-- Maximally insert return types
maxinsert :: CType -> CExpr -> CExpr
-- Unwrap proc types to contained type
maxinsert t c@CExprCall { .. }
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

