{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Pipeline where
import Data.Text (Text)
import Codegen.Expr
import Codegen.Utils
import Codegen.Defs
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

-- Natural semantics
natSemantics :: CExpr -> CExpr
-- Semantics for O and S
natSemantics CExprCall { _fname = "Datatypes.O", _fparams = [] } = CExprNat 0
natSemantics CExprCall { _fname = "Datatypes.O", _fparams = [args] } = error "Datatypes.0 with args found!"
natSemantics CExprCall { _fname = "Datatypes.S", _fparams = [a] } = CExprNat $ (_nat . natSemantics $ a) + 1
natSemantics CExprCall { _fname = "Datatypes.S", _fparams = a:arg } = error "Datatypes.S with more than one args found!"
-- Propagate to children expr
natSemantics CExprLambda { .. } = CExprLambda _largs (natSemantics _lbody)
natSemantics CExprCase   { .. } = CExprCase (natSemantics _cexpr) (map natSemantics _cases)
natSemantics CExprMatch  { .. } = CExprMatch (natSemantics _mpat) (natSemantics _mbody)
natSemantics CExprCall   { .. } = CExprCall _fname (map natSemantics _fparams)
natSemantics CExprTuple  { .. } = CExprTuple (map natSemantics _items)
-- Transparent to the rest of expressions
natSemantics other = other

boolSemantics :: CExpr -> CExpr
-- Semantics for True and False
boolSemantics CExprCall { _fname = "Datatypes.Coq_true", _fparams = [] } = CExprBool True
boolSemantics CExprCall { _fname = "Datatypes.Coq_false", _fparams = [] } = CExprBool False
-- Propagate to children expr
boolSemantics CExprLambda { .. } = CExprLambda _largs (boolSemantics _lbody)
boolSemantics CExprCase   { .. } = CExprCase (boolSemantics _cexpr) (map boolSemantics _cases)
boolSemantics CExprMatch  { .. } = CExprMatch (boolSemantics _mpat) (boolSemantics _mbody)
boolSemantics CExprCall   { .. } = CExprCall _fname (map boolSemantics _fparams)
boolSemantics CExprTuple  { .. } = CExprTuple (map boolSemantics _items)
-- Transparent to the rest of expressions
boolSemantics other = other


-- List semantics
listSemantics :: CExpr -> CExpr
-- Semantics for O and S
listSemantics CExprCall { _fname = "Datatypes.Coq_nil", _fparams = [] } = CExprList []
listSemantics CExprCall { _fname = "Datatypes.Coq_nil", _fparams = [args] } = error "Datatypes.Coq_nil with args found!"
listSemantics CExprCall { _fname = "Datatypes.Coq_cons", _fparams = [a, b] } = CExprList $ (semantics a):(_elems . listSemantics $ b)
listSemantics CExprCall { _fname = "Datatypes.Coq_cons", _fparams = a:b:arg } = error "Datatypes.Coq_cons with more than two args found!"
-- Propagate to children expr
listSemantics CExprLambda { .. } = CExprLambda _largs (listSemantics _lbody)
listSemantics CExprCase   { .. } = CExprCase (listSemantics _cexpr) (map listSemantics _cases)
listSemantics CExprMatch  { .. } = CExprMatch (listSemantics _mpat) (listSemantics _mbody)
listSemantics CExprCall   { .. } = CExprCall _fname (map listSemantics _fparams)
listSemantics CExprTuple  { .. } = CExprTuple (map listSemantics _items)
-- Transparent to the rest of expressions
listSemantics other = other

{-
  "tag": "CExprCall",
                        "_fname": "Coq_bind",
                        "_fparams": [
                            {
                                "tag": "CExprCall",
                                "_fname": "Coq_open",
                                "_fparams": [
                                    {
                                        "tag": "CExprCall",
                                        "_fname": "append",
                                        "_fparams": [
                                            {
                                                "tag": "CExprStr",
                                                "_str": "path"
                                            },
                                            {
                                                "tag": "CExprStr",
                                                "_str": "fn"
                                            }
                                        ]
                                    }
                                ]
                            },
                            {
                                "_largs": [
                                    {
                                        "_name": "f",
                                        "_typename": "auto"
                                    }
                                ],
                                "tag": "CExprLambda",
                                "_lbody": {
                                    "tag": "CExprCall",
                                    "_fname": "Coq_bind",
                                    "_fparams": [
                                        {

-}

-- TODO:
-- Proc semantics (monadic)
procSemantics :: CExpr -> CExpr
procSemantics CExprCall { _fname = "Coq_bind", _fparams = [arg] } = error "Datatypes.Coq_bind with one arg found, undefined behavior."
procSemantics CExprCall { _fname = "Coq_bind", _fparams = [a,
    CExprLambda { .. }] } = CExprSeq (CExprStmt typenm nm a) $ procSemantics _lbody
    where unidef []     = ("void", "fake_name")
          unidef [d@CDef { .. }]    = (_typename, _name)
          unidef (a:ls) = error "Bind to multiple arguments not allowed, failing"
          typenm = fst . unidef $ _largs
          nm = snd . unidef $ _largs
procSemantics CExprCall { _fname = "Coq_bind", _fparams = a:b:arg } = error "Datatypes.Coq_bind with more than two args found!"
procSemantics CExprCall { _fname = "Coq_ret", .. } = CExprStr "return" -- implement return arguments
procSemantics CExprLambda { .. } = CExprLambda _largs (procSemantics _lbody)
procSemantics CExprCase   { .. } = CExprCase (procSemantics _cexpr) (map procSemantics _cases)
procSemantics CExprMatch  { .. } = CExprMatch (procSemantics _mpat) (procSemantics _mbody)
procSemantics CExprCall   { .. } = CExprCall _fname (map procSemantics _fparams)
procSemantics CExprTuple  { .. } = CExprTuple (map procSemantics _items)
-- Transparent to the rest of expressions
procSemantics other = other

-- Update all semantic transforms here to create a pipeline
semantics = procSemantics . listSemantics . natSemantics . boolSemantics

