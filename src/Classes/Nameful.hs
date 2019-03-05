{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Classes.Nameful where
import CIR.Decl
import CIR.Expr
import Data.Text (Text)
import qualified Data.Text  as T

-- This class is for instances with names
class Nameful a where
    getname :: a -> Text

-- Has a name
instance Nameful CDecl where
    getname CDFunc   { .. } = _nm _fd
    getname CDType   { .. } = _nm _td
    getname CDStruct { .. } = _sn
    getname CDEmpty  {} = ""
    getname CDSeq    { .. } = getname _left

instance Nameful CDef where
    getname CDef { .. } = _nm

instance Nameful CExpr where
    getname CExprCall   { .. } = _nm _cd
    getname CExprStr    { .. } = _str
    getname CExprNat    { .. } = T.pack . show $ _nat
    getname CExprStmt   { .. } = _nm _sd
    getname CExprVar    { .. } = _var
    getname CExprBool   { .. } = T.pack . show $ _bool
    getname e = error $ "Cannot get a name for expr " ++ show e

instance Nameful CType where
    getname CTExpr { .. } = _tbase
    getname CTBase { .. } = _base
    getname CTVar  { .. } = _vname
    getname t = error $ "Cannot get a name for type " ++ show t

