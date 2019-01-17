{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Types.Templates where
import Types.Context
import CIR.Expr
import Data.Map
import Data.MonoTraversable

-- Make a CDef into a template form expected by cc
class Template a where
    templatify :: Context CType -> a -> a
    plug :: Int -> a -> a

instance Template CDef where
    templatify ctx CDef { .. } = case ctx !? _nm of
        (Nothing) -> CDef _nm _ty
        (Just t) -> case gcs t _ty of
            (Nothing) -> CDef _nm _ty
            (Just t) -> CDef _nm t

    plug n CDef { .. } = CDef _nm $ plug n _ty

instance Template CExpr where
    templatify ctx CExprCall { .. } = CExprCall (templatify ctx _cd) $ fmap (templatify ctx) _cparams
    templatify ctx e = omap (templatify ctx) e

    plug n CExprCall   { .. } = CExprCall (plug n _cd) $ fmap (plug n) _cparams
    plug n CExprStmt   { .. } = CExprStmt (plug n _sd) $ plug n _sbody
    plug n CExprLambda { .. } = CExprLambda (fmap (plug n) _lds) $ plug n _lbody
    plug n e = omap (plug n) e

instance Template CType where
    templatify _ t = t

    plug n CTFree { .. } | n < _idx = CTAuto
    plug n t = omap (plug n) t

-- Retype high order functions to CTAuto so they do not show a template
highorder :: [CDef] -> CExpr -> CExpr
highorder ds CExprCall { _cd = CDef { _nm = fn, .. }, .. }
    | fn `elem` fmap _nm ds = CExprCall (CDef fn CTAuto) $ fmap (highorder ds) _cparams
highorder ds e = omap (highorder ds) e

-- Greatest common subtype between two types
gcs :: CType -> CType -> Maybe CType
gcs CTFree { _idx = a  } CTFree { _idx = b } | a == b = Just $ CTFree a
gcs CTFree { .. } t = Just t
gcs _ CTBase { .. } = Just CTAuto
gcs CTBase { .. } _ = Just CTAuto
gcs CTExpr { _tbase = a, _tins = [as]  } CTExpr { _tbase = b, _tins = [bs] } | a == b = gcs as bs
gcs CTFunc { _fret = ra } CTFunc { _fret = rb } = gcs ra rb
gcs CTFunc { .. } t = gcs _fret t
gcs CTPtr  { _inner = a } CTPtr { _inner = b } = gcs a b
gcs _ _ = Nothing


