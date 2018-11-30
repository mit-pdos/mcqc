{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Inference where
import CIR.Expr
import CIR.Decl
import Data.Text (Text)
import Data.MonoTraversable
import Control.Lens (view)
import Data.Map.Strict (Map)
import qualified Data.Map      as M

-- Named context
type Context a = Map Text [a]

-- Print key values correctly
printCtx :: Show a => Context a -> IO ()
printCtx = putStr . concatMap (++"\n") . M.elems . M.mapWithKey (\k v -> show k ++ " : " ++ show v)

-- Merge two contexts, throw an error if conflicting definitions happen (no overloading)
mergeCtx :: Context CType -> Context CType -> Context CType
mergeCtx = M.unionWithKey (\k va vb ->
    error $ "Constructor " ++ show k ++ " has conflicting definitions " ++ show va  ++ "\n" ++ show vb)

-- Append a Decl in a context
addCtx :: Context CType -> CDecl -> Context CType
addCtx ctx CDFunc { _fd = CDef { .. }, .. } = M.insert _nm (map (view ty) _fargs ++ [_ty]) ctx
addCtx ctx CDType { _td = CDef { .. }, .. } = M.insert _nm [_ty] ctx
addCtx ctx CDInd  { _id = CDef { .. }, .. } = M.insert _nm [_ty] ctx -- TODO: Add _ictors to context too
addCtx ctx _ = ctx

-- Unify two types
unify :: CType -> CType -> CType
-- Any type is better than undefined type
unify a b | a == b = b
unify t CTUndef {} = t
unify CTUndef {} t = t
unify CTFunc { _fret = a, _fins = ina} CTFunc { _fret = b, _fins = inb}
    | length ina == length inb = CTFunc (unify a b) $ zipWith unify ina inb
    | otherwise = error $ "Attempting to unify func types with different args" ++ show ina ++ " " ++ show inb
-- Ignore Proc monad wrapped types
unify CTExpr { _tbase = "proc" , _tins = [a] } t = unify a t
unify t CTExpr { _tbase = "proc" , _tins = [a] } = unify t a
unify CTExpr { _tbase = a , _tins = ina } CTExpr { _tbase = b , _tins = inb }
    | a == b && length ina == length inb = CTExpr a $ zipWith unify ina inb
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

-- Maximally plug a type into an expression, given a type context (Gamma)
unifyExpr :: Context CType -> CType -> CExpr -> CExpr
unifyExpr ctx t CExprCall { .. }
    -- Return preserves the type
    | _fname == "return" = CExprCall "return" $ map (unifyExpr ctx t) _fparams
    -- A match preserves the type if the lambdas return it (omit matched object)
    | _fname == "match"  = CExprCall "match" $ head _fparams:map (unifyExpr ctx t) (tail _fparams)
    -- | _fname == "app"    = CExprCall "app" $ map (unifyExpr ctx t) _fparams
    | _fname `M.member` ctx =
        let params = zipWith (\tp exp -> unifyExpr ctx tp exp) (ctx M.! _fname) _fparams in
            CExprCall _fname params
    -- Function call obfuscate the return type, ignore them
    | otherwise          = CExprCall _fname _fparams
-- Or explicit if it comes from the first rule handling return calls
unifyExpr ctx t s@CExprSeq { .. } = listToSeq $ first ++ [retexpr]
    where retexpr = unifyExpr ctx t . last . seqToList $ s
          first   = init . seqToList $ s
unifyExpr ctx t o = omap (unifyExpr ctx t) o

