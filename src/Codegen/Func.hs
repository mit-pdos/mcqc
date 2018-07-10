{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, DuplicateRecordFields, TypeSynonymInstances, RecordWildCards, FlexibleInstances  #-}
module Codegen.Func (CFunc, toCFunc) where
import GHC.Generics
import Codegen.Rewrite (toCType, toCName)
import Codegen.Expr
import Codegen.Defs
import Codegen.Pattern
import Codegen.Utils
import Parser.Decl
import Sema.Pipeline
import Parser.Fix
import Parser.Expr
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

-- C function definition, ie: int main(int argc, char **argv)
data CFunc = CFunc { fname :: Text, ftype :: Text, fargs :: [CDef], fvars :: [Text], fbody :: CExpr }
  deriving (Eq, Generic, ToJSON)

instance Pretty CFunc where
  pretty CFunc { .. } = pretty ftype <+> mkFuncSig fname (map pretty fargs)
                    <> vcat ["{", tab mainbody, "}"]
                    <> line
    where mainbody = "var<int>" <+> concatWith (surround ", ") (map pretty fvars) <> ";"
                      <> line
                      <> "return" <+> (pretty . semantics) fbody

-- Nat -> Nat -> Bool ==> [Nat, Nat, Bool]
getCTypeList :: Typ -> [Text]
getCTypeList TypArrow { left = lt, right = rt } = (getCTypeList lt) ++ (getCTypeList rt)
getCTypeList TypVar { name = n, args = al } = ["<getCTypeLast PLACEHOLDER>"]
getCTypeList TypGlob { name = n } = [toCType n]

-- Nat -> Nat -> Bool ==> Bool
getCRetType :: Typ -> Text
getCRetType = last . getCTypeList

-- Get the argument names from lamda definition
getCNames :: Expr -> [Text]
getCNames ExprLambda { argnames = al } = map toCName al
getCNames ExprRel {..} = [toCName name]
getCNames ExprGlobal {..} = [toCName name]

-- Need to initialize vars used in matches, find them now
getCaseMatchVars :: CPattern -> [Text]
getCaseMatchVars CPatCtor { .. } = argnames
getCaseMatchVars CPatTuple { .. } = concat $ map getCaseMatchVars items
-- XXX: Very hackish
getCaseVars :: CExpr -> [Text]
getCaseVars e@CExprCase { cases = [], .. } = []
getCaseVars e@CExprCase { cases = m:ms, .. } = getCaseVars m ++ getCaseVars smaller
    where smaller = CExprCase (CExprStr "foobar") ms
getCaseVars e@CExprMatch { .. } = getCaseMatchVars mpat ++ getCaseVars mbody
getCaseVars CExprStr     { .. } = [str]
getCaseVars CExprNat     { .. } = [T.pack . show $ nat]
getCaseVars CExprListNat { .. } = map (T.pack . show) nats
getCaseVars CExprListStr { .. } = strs
getCaseVars _ = []


-- Fixpoint declaration to C Function
toCFunc :: Fix -> CFunc
toCFunc Fix { name = Just n, typ = t, value = l@ExprLambda {..} } = CFunc n rettype defs varnames cbody
    where defs = getCDefExtrap args argtypes
          varnames = getCaseVars cbody
          cbody = toCExpr body
          args = getCNames l
          argtypes = (init . getCTypeList) t
          rettype = getCRetType t
toCFunc Fix { name = Nothing, typ = t, value = l@ExprLambda {..} } = CFunc "NoNamePlaceholder" rettype defs varnames cbody
    where defs = getCDefExtrap args argtypes
          varnames = getCaseVars cbody
          cbody = toCExpr body
          args = getCNames l
          argtypes = (init . getCTypeList) t
          rettype = getCRetType t


