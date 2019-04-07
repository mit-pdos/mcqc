{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Classes.Typeful where
import Classes.Nameful
import CIR.Expr
import CIR.Decl
import Types.Context
import Codegen.Rewrite
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Map  as M
import qualified Data.Text as T

-- This class is for instances with types
class Typeful a where
    -- Get all libraries needed by a
    getincludes  :: a -> [Text]
    -- Unify with a type (inference) with a Type context
    unify        :: Context CType -> CType -> a -> Maybe a
    -- Return the type
    gettype      :: a -> CType
    -- Add types to context
    addctx       :: Context CType -> a -> Context CType
    -- Get number of free types
    getMaxVaridx :: a -> Int
    -- Can it be unified with something from Context
    isUnifiable :: Context CType -> a -> Bool

-- Has a type
instance Typeful CDecl where
    getincludes CDEmpty  {}     = []
    getincludes CDType   { .. } = getincludes _td
    getincludes CDFunc   { .. } = getincludes _fd ++ concatMap getincludes _fargs ++ getincludes _fbody
    getincludes CDStruct { .. } = concatMap getincludes _fields
    getincludes CDSeq    { .. } = getincludes _left ++ getincludes _right

    unify ctx t CDType   { .. } = CDType <$> unify ctx t _td
    unify ctx t CDFunc   { .. } = CDFunc <$> unify ctx t _fd
                                         <*> pure  _fargs
                                         <*> unify ctx t _fbody
    unify ctx t CDStruct { .. } = CDStruct _sn <$> mapM (unify ctx t) _fields
                                               <*> pure _nfree
    unify ctx t CDSeq    { .. } = CDSeq <$> unify ctx t _left
                                        <*> unify ctx t _right
    unify _ _  a = pure a

    gettype CDType { .. } = gettype _td
    gettype CDFunc { .. } = map gettype _fargs --> _ty _fd
    gettype _             = CTAuto

    addctx ctx CDFunc { _fd = CDef { _nm = "match" } } = ctx
    addctx ctx d@CDFunc { .. } = mergeCtx ctx $ M.singleton (_nm _fd) (gettype d)
    addctx ctx CDType { _td = CDef { _ty = CTExpr { _tbase = "std::variant", ..  }, .. } }
        | freedom > 0 = mergeCtx ctx $ M.fromList . map exprmaker $ _tins
        | otherwise = mergeCtx ctx $ M.fromList . map basemaker $ _tins
        where freedom = maximum . map getMaxVaridx $ _tins
              exprmaker n = (getname n, CTExpr _nm [CTFree i | i <- [1..freedom]])
              basemaker n = (getname n, CTBase _nm)
    addctx ctx CDType { .. } = addctx ctx _td
    addctx ctx CDSeq  { .. } = ctx `addctx` _left `addctx` _right
    addctx ctx _ = ctx

    getMaxVaridx = getMaxVaridx . gettype

    isUnifiable ctx d = (cannonicalizeFn . getname $ d) `M.member` ctx
                        && isUnifiable ctx (gettype d)

instance Typeful CDef where
    getincludes CDef     { .. } = getincludes _ty
    gettype CDef         { .. } = _ty
    unify ctx t CDef     { .. } = CDef _nm <$> unify ctx t _ty
    addctx ctx CDef      { .. } = mergeCtx ctx $ M.singleton _nm _ty
    isUnifiable ctx CDef { .. } = _nm `M.member` ctx && isUnifiable ctx _ty
    getMaxVaridx = getMaxVaridx . gettype

instance Typeful CExpr where
    getincludes CExprSeq    { .. } = "io" : getincludes _left ++ getincludes _right
    getincludes CExprCall   { _cd = CDef { _nm = "show" , ..}, .. } = "show" : getincludes _ty ++ concatMap getincludes _cparams
    getincludes CExprCall   { _cd = CDef { _nm = "gmatch" }, .. } = "variant" : concatMap getincludes _cparams
    getincludes CExprCall   { _cd = CDef { .. }, .. } = _nm : getincludes _ty ++ concatMap getincludes _cparams
    getincludes CExprStr    { .. } = ["String"]
    getincludes CExprNat    { .. } = ["nat"]
    getincludes CExprPair   { .. } = "pair" : getincludes _fst ++ getincludes _snd
    getincludes CExprStmt   { .. } = "io" : getincludes _sd ++ getincludes _sbody
    getincludes CExprLambda { .. } = concatMap getincludes _lds ++ getincludes _lbody
    getincludes CExprBool   { .. } = ["bool"]
    getincludes CExprVar    { .. } = []

    gettype CExprSeq    { .. } = gettype . last . seqToList $ _right
    gettype CExprCall   { .. } = gettype _cd
    gettype CExprStr    { .. } = CTBase "string"
    gettype CExprNat    { .. } = CTBase "nat"
    gettype CExprPair   { .. } = CTExpr "pair" $ [gettype i | i <- [_fst, _snd]]
    gettype CExprStmt   { .. } = gettype _sd
    gettype CExprLambda { .. } = gettype _lbody
    gettype CExprBool   { .. } = CTBase "bool"
    gettype _ = CTAuto

    unify ctx t CExprCall { _cd = CDef { .. },  .. }
        -- Return preserves the type
        | _nm == "return" = CExprCall <$> newD
                                      <*> mapM (unify ctx t) _cparams
        -- A match preserves the type if the lambdas return it (omit matched object)
        | _nm  == "match" = CExprCall <$> newD
                                      <*> Just (head _cparams : fromMaybe [] (mapM (unify ctx t) (tail _cparams)))
        -- Match with something from the context
        | otherwise =
            case ctx M.!? _nm of
              (Just CTFunc { .. }) -> CExprCall <$> newD
                                                <*> zipWithM (unify ctx) _fins _cparams
              (_) ->  CExprCall <$> newD
                                <*> pure _cparams
        where newD = CDef _nm <$> unify ctx t _ty
    -- Or explicit if it comes from the first rule handling return calls
    unify ctx t s@CExprSeq { .. } = retexpr >>= (\t -> pure $ listToSeq (first ++ [t]))
        where retexpr = unify ctx t . last . seqToList $ s
              first   = init . seqToList $ s
    unify ctx t o = omapM (unify ctx t) o

    -- Cowardly refuse to add expression to global context
    addctx ctx _ = ctx

    -- Get max varidx by getting the type first
    getMaxVaridx = getMaxVaridx . gettype

    -- The name is in the context and the type is unifiable
    isUnifiable ctx c@CExprCall { _cd = CDef { .. }, .. } = ((cannonicalizeFn _nm) `M.member` ctx) && (isUnifiable ctx . gettype $ c)
    isUnifiable ctx e = isUnifiable ctx . gettype $ e

instance Typeful CType where
    getincludes CTFunc { .. } = getincludes _fret ++ concatMap getincludes _fins
    getincludes CTExpr { .. } = T.toLower _tbase : concatMap getincludes _tins
    getincludes CTVar  { .. } = concatMap getincludes _vargs
    getincludes CTBase { .. } = [T.toLower _base]
    getincludes CTPtr  { .. } = getincludes _inner
    getincludes _             = []

    -- Unify the same type
    unify _ a b | a == b = Just b
    -- Type is better than auto-type
    unify _ t CTAuto = Just t
    unify _ CTAuto _ = Just CTAuto
    -- Function types
    unify c CTFunc { _fret = a, _fins = ina} CTFunc { _fret = b, _fins = inb }
        | length ina == length inb = zipWithM (unify c) ina inb >>= (\uargs -> unify c a b >>= (\ret -> Just (uargs --> ret)))
        | otherwise = Nothing -- error $ "Attempting to unify func types with different args" ++ show ina ++ " " ++ show inb
    -- Ignore IO monad wrapped types
    unify c CTExpr { _tbase = "IO" , _tins = [a] } t = unify c a t
    unify c t CTExpr { _tbase = "IO" , _tins = [a] } = unify c t a
    -- Unify composite type expressions
    unify c CTExpr { _tbase = a , _tins = ina } CTExpr { _tbase = b , _tins = inb }
        | a == b && length ina == length inb = (Just . CTExpr a) =<< zipWithM (unify c) ina inb
        | otherwise = Nothing -- error $ "Attempting to unify list types with different args" ++ show ina ++ " " ++ show inb
    -- Unify free parameters, here we're assuming Coq has already type-checked this
    unify _ CTFree { .. } t = Just t
    unify _ t CTFree { .. } = Just t
    -- Pointers go down, not up
    unify c CTPtr { .. } t = CTPtr <$> unify c _inner t
    unify _ _ _ = Nothing

    -- Return the type itself
    gettype x = x

    -- addctx will not do anything without a name
    addctx ctx _ = ctx

    -- Return number of free variables
    getMaxVaridx t = foldl max 0 $ getVaridxs t
        where getVaridxs CTFree { .. } = [_idx]
              getVaridxs CTFunc { .. } = getVaridxs _fret ++ concatMap getVaridxs _fins
              getVaridxs CTExpr { .. } = concatMap getVaridxs _tins
              getVaridxs CTPtr  { .. } = getVaridxs _inner
              getVaridxs _ = [0]

    -- Is the type unifiable with anything in the context
    isUnifiable ctx t = not . null . mapMaybe (unify ctx t) . M.elems $ ctx
