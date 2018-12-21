{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Types.Templates where
import Types.Context
import CIR.Expr
import Data.Map
import Data.MonoTraversable
import Debug.Trace

-- Make a CDef into a template form by getting the gcs
class Template a where
    templatify :: Context CType -> a -> a

instance Template CDef where
    templatify ctx CDef { .. } = case ctx !? _nm of
        (Nothing) -> CDef _nm _ty
        (Just t) -> case gcs t _ty of
            (Nothing) -> CDef _nm _ty
            (Just t) -> CDef _nm t

instance Template CExpr where
    templatify ctx d | trace ("Templating expr " ++ show d) False = undefined
    templatify ctx CExprCall { .. } = trace ("=====  GOT IT " ++ show _cd ++ " becomes " ++ show (templatify ctx _cd)) (CExprCall (templatify ctx _cd) $ fmap (templatify ctx) _cparams)
    templatify ctx e = omap (templatify ctx) e

-- Greatest common subtype between two types
gcs :: CType -> CType -> Maybe CType
gcs CTFree { _idx = a  } CTFree { _idx = b } | a == b = Just $ CTFree a
gcs CTFree { .. } t = Just t
gcs CTExpr { _tbase = a, _tins = [as]  } CTExpr { _tbase = b, _tins = [bs] } | a == b = gcs as bs
gcs CTFunc { _fret = ra } CTFunc { _fret = rb } = gcs ra rb
gcs CTFunc { .. } t = gcs _fret t
gcs CTPtr  { _inner = a } CTPtr { _inner = b } = gcs a b
gcs _ _ = Nothing


