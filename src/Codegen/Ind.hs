{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Codegen.Ind where
import Classes.Typeful
import Classes.Nameful
import Parser.Decl
import Parser.Expr
import CIR.Expr
import CIR.Decl
import Codegen.Rewrite
import Common.Utils
import Data.Text (Text)
import qualified Data.Text     as T
import qualified Data.Maybe    as MA
import qualified Data.List     as L

-- Intermadiate representation for inductive datatypes
data CDInd = CDInd { _id :: CDef, _ictors :: [CDef] }

-- Transcribe to CType with a list of abstractors
toCTypeAbs :: [Text] -> Type -> CType
toCTypeAbs abs TVar  { .. }
    | name `elem` abs = CTFree . (+1) . MA.fromJust . L.elemIndex name $ abs
    | otherwise = CTVar (toCTBase name) []
toCTypeAbs _ TGlob    { targs = [], .. } = CTBase $ toCTBase name
toCTypeAbs abs TGlob         { .. } = CTExpr (toCTBase name) $ map (toCTypeAbs abs) targs
toCTypeAbs abs TVaridx       { .. } = CTFree $ idx + length abs
toCTypeAbs _ TDummy          {}   = CTBase "void"
toCTypeAbs _ TUnknown        {}     = CTAuto
toCTypeAbs abs t@TArrow { .. }      = (init . flattenType $ t) --> (last . flattenType $ t)
    where flattenType TArrow { .. } = toCTypeAbs abs left:flattenType right
          flattenType t             = [toCTypeAbs abs t]

-- Inductive contructor to CDef, with abstractors
mkctor :: [Text] -> CType -> Ind -> CDef
mkctor abs indtype Ind { .. } = CDef name ctyp
    where ctyp = map transT argtypes --> indtype
          transT = mkptr . toCTypeAbs abs
          mkptr t | t == indtype = CTPtr t | otherwise = t

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
    where freedom = maximum $ map getMaxVaridx _ictors
          unaliasedT = CTExpr "std::variant" $ mapf (toCtorT freedom) _ictors
          mconcatMap f a = mconcat $ map f a

-- Take a Coq constructor, create a C++ constructor type
toCtorT :: Int -> Text -> CType -> CType
toCtorT n nm CTFunc { .. }
    | freedom > 0 = CTExpr nm [CTFree i | i<- [1..freedom]]
    | otherwise = CTBase nm
    where freedom = maximum $ n:map getMaxVaridx _fins

-- Make a lambda clause to a match
mkMatchClause :: Int -> Text -> CDef -> CExpr
mkMatchClause nfree fn CDef { _ty = ft@CTFunc { .. }, .. } =
    CExprLambda [CDef "_" $ toCtorT nfree _nm ft] $ CExprCall (CDef fn CTAuto) (dodots $ length _fins)
    where dodots n = [CExprVar ("_" `T.append` "." `T.append` T.pack [i]) | i <- take n ['a'..]]

-- Make a match statement for unfolding the Inductive type
mkMatch :: CDef -> [CDef] -> CDecl
mkMatch CDef { .. } ctors =
    CDFunc (CDef "match" CTAuto) ((CDef "self" $ CTPtr _ty):fdefs) $
      CExprCall (CDef "return" CTAuto) [
        CExprCall (CDef "gmatch" CTAuto) $ (CExprVar "self"):(zipWith mkClause fnames ctors)
      ]
    where freedom = getMaxVaridx _ty
          mkClause = mkMatchClause freedom
          fdefs = givenm 'f' [CTFree (i + freedom) | i <- [1..length ctors]]
          fnames = map getname fdefs

-- Make a struct for each Coq inductive constructor with that name
mkCtorStruct :: CType -> CDef -> CDecl
mkCtorStruct unaliasT CDef { _nm = name, _ty = CTFunc { .. } } =
    CDStruct name (givenm 'a' . map mkRecTypes $ _fins) freedom
    where freedom = getMaxVaridx unaliasT
          mkRecTypes rec
            | CTPtr _fret == rec = CTPtr unaliasT
            | otherwise = rec
mkCtorStruct _ CDef { .. } = error $ "Cannot export constructor " ++ show (_nm)

-- Make C++ variant an alias for Coq inductive type
mkIndAlias :: CDef -> [CDef] -> CDecl
mkIndAlias CDef { .. } = CDType . CDef _nm . CTExpr "std::variant" . mapf (toCtorT freedom)
    where freedom = getMaxVaridx _ty

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
