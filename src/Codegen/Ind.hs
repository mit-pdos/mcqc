{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Codegen.Ind where
import Parser.Decl
import Parser.Expr
import CIR.Expr
import CIR.Decl
import Codegen.Expr
import Codegen.Rewrite
import Common.Utils
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Common.Config as Conf
import qualified Data.Text     as T
import Debug.Trace

-- Intermadiate representation for inductive datatypes
data CDInd = CDInd { _id :: CDef, _ictors :: [CDef] }

-- Inductive contructor to CDef, with abstractors
mkctor :: [Text] -> CType -> Expr -> CDef
mkctor abs indtype IndConstructor { .. } = CDef name ctyp
    where ctyp = CTFunc indtype . map transT $ argtypes
          transT = mkptr . toCTypeAbs abs
          mkptr t | t == indtype = CTPtr t | otherwise = t
mkctor _ _ o = error $ "Non inductive constructor found, failing " ++ show o

-- Expand inductive declaration from Coq to intermediate representation CDInd
expand :: Declaration -> CDInd
-- (Parametric) Inductive type
expand IndDecl { .. } = CDInd (CDef iname indtype) $ map mkctorD constructors
    where mkctorD = mkctor iargs indtype
          indtype = case iargs of
                        ([]) -> CTBase iname
                        (_)  -> CTExpr iname [CTFree $ length iargs]
          iname = toCTBase name

-- Contract indermediate representation to multiple concatenated CDecl
contract :: CDInd -> CDecl
contract CDInd  { .. } = mconcatMap (mkCtorStruct unaliasedT) _ictors
                        <> mkIndAlias _id _ictors
                        <> mconcatMap (mkCtorFunc _id) _ictors
                        <> mkMatch _id _ictors
    where unaliasedT = CTExpr "std::variant" $ mapf toCtorT _ictors
          mconcatMap f a = mconcat $ map f a

-- Take a Coq constructor, create a C++ constructor type
toCtorT :: Text -> CType -> CType
toCtorT nm CTFunc { .. }
    | freedom > 0 = CTExpr nm [CTFree freedom]
    | otherwise = CTBase nm
    where freedom = maximum $ 0:map getMaxVaridx _fins
-- Make a lambda clause to a match
mkMatchClause :: Text -> CDef -> CExpr
mkMatchClause fn CDef { _ty = ft@CTFunc { .. }, .. } =
    CExprLambda [CDef "_" $ toCtorT _nm ft] $ CExprCall (CDef fn CTAuto) (dodots $ length _fins)
    where dodots n = [CExprVar ("_" `T.append` "." `T.append` T.pack [i]) | i <- take n ['a'..]]

-- Make a match statement for unfolding the Inductive type
mkMatch :: CDef -> [CDef] -> CDecl
mkMatch CDef { .. } ctors =
    CDFunc (CDef "match" CTAuto) ((CDef "self" $ CTPtr _ty):fdefs) $
      CExprCall (CDef "return" CTAuto) [
        CExprCall (CDef "gmatch" CTAuto) $ (CExprVar "self"):(zipWith mkMatchClause fnames ctors)
      ]
    where freevars = getMaxVaridx _ty
          fdefs = givenm 'f' [CTFree (i + freevars) | i <- [1..length ctors]]
          fnames = map getname fdefs

-- Make a struct for each Coq inductive constructor with that name
-- Args: constructor: (Text, CType)
--       Struct recursive type (own type unaliased)
mkCtorStruct :: CType -> CDef -> CDecl
mkCtorStruct unaliasT CDef { _nm = name, _ty = CTFunc { .. } } =
    CDStruct name (givenm 'a' . map mkRecTypes $ _fins)
    where mkRecTypes rec
            | CTPtr _fret == rec = CTPtr unaliasT
            | otherwise = rec
mkCtorStruct _ CDef { .. } = error $ "Cannot export constructor " ++ show (_nm)

-- Make C++ variant an alias for Coq inductive type
mkIndAlias :: CDef -> [CDef] -> CDecl
mkIndAlias CDef { .. } = CDType . CDef _nm . CTExpr "std::variant" . mapf toCtorT

-- Make C++ ctor functions for each Coq inductive constructor
mkCtorFunc :: CDef -> CDef -> CDecl
mkCtorFunc CDef { .. } CDef { _nm = ctornm, _ty = CTFunc { .. } } =
    CDFunc fdptr defs $
      CExprCall (CDef "return" (CTPtr _ty)) [    -- return inside the ctor body
        CExprCall (CDef "std::make_shared" _ty) [ -- wrap in a shared pointer
          CExprCall fd . map (CExprVar . getname) $ defs -- Call struct constructor in mkCDStruct
        ]
      ]
    where fdptr = CDef (T.toLower ctornm) (CTPtr _ty)
          fd = CDef ctornm _ty
          defs  = givenm 'a' _fins

