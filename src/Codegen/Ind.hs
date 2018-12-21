{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Codegen.Ind where
import CIR.Expr
import CIR.Decl
import Common.Utils
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Common.Config as Conf
import qualified Data.Text     as T
import Debug.Trace

-- Take a constructor name and a type, create a constructor type for that name
toCtorT :: Text -> CType -> CType
toCtorT nm CTFunc { .. } =
    if freedom > 0 then CTExpr nm [CTFree freedom] else CTBase nm
    where freedom = maximum $ 0:map getMaxVaridx _fins

-- Wrap non-base types to a pointer
addPtr :: CType -> CType
addPtr t@CTBase { .. }
    | _base `elem` Conf.base = t
    | otherwise = CTPtr t
addPtr t@CTExpr { .. }
    | _tbase `elem` Conf.base = t
    | otherwise = CTPtr t
addPtr t@CTVar { .. }
    | _vname `elem` Conf.base = t
    | otherwise = CTPtr t
addPtr t = t

-- Make a match statement for unfolding the Inductive type
mkMatch :: CDef -> [(Text, CType)] -> CDecl
mkMatch CDef { .. } ctors =
    CDFunc (CDef "match" CTAuto) ((CDef "self" $ CTPtr _ty):fdefs) $
      CExprCall (CDef "return" CTAuto) [
        CExprCall (CDef "gmatch" CTAuto) $ (CExprVar "self"):(zipWith mkLambdas fnames  ctors)
      ]
    where freevars                      = getMaxVaridx _ty
          fdefs                         = givenm 'f' [CTFree (i + freevars) | i <- [1..length ctors]]
          fnames                        = map getname fdefs
          mkLambdas f (o,ft@CTFunc { .. }) = CExprLambda [CDef "_" $ toCtorT o ft] $ CExprCall (CDef f CTAuto) (dodots $ length _fins)
          dodots n                      = [CExprVar ("_" `T.append` "." `T.append` T.pack [i]) | i <- take n ['a'..]]

-- Make a struct for each Coq inductive constructor with that name
-- Args: constructor: (Text, CType)
--       Struct recursive type (own type unaliased)
mkCtorStruct :: CType -> (Text, CType) -> CDecl
mkCtorStruct unaliasT (name, CTFunc { .. }) = CDStruct name (givenm 'a' . map (mkRecTypes _fret) $ _fins)
    where mkRecTypes t rec | CTPtr t == rec = CTPtr unaliasT | otherwise = rec
mkCtorStruct _ (name, o) = error $ "Cannot export constructor" ++ show name ++ " of " ++ show o

-- Make C++ variant an alias for Coq inductive type
mkIndAlias :: CDef -> [(Text, CType)] -> CDecl
mkIndAlias CDef { .. } = CDType . CDef _nm . CTExpr "std::variant" . map (\(cn, t) -> toCtorT cn t)

-- Make C++ ctor functions for each Coq inductive constructor
mkCtorFunc :: CDef -> (Text, CType) -> CDecl
mkCtorFunc CDef { .. } (ctornm, CTFunc { .. }) =
    CDFunc fdptr defs $
      CExprCall (CDef "return" (CTPtr _ty)) [    -- return inside the ctor body
        CExprCall (CDef "std::make_shared" _ty) [ -- wrap in a shared pointer
          CExprCall fd . map (CExprVar . getname) $ defs -- Call struct constructor in mkCDStruct
        ]
      ]
    where fdptr = CDef (T.toLower ctornm) (CTPtr _ty)
          fd = CDef ctornm _ty
          defs  = givenm 'a' _fins

-- Intermediate representation before printing inductive as tagged-union, the order of the list is important
expandind :: CDecl -> [CDecl]
expandind CDInd { .. }  = map (mkCtorStruct unaliasedT) _ictors ++ [mkIndAlias _id _ictors] ++ map (mkCtorFunc _id) _ictors ++ [mkMatch _id _ictors]
    where unaliasedT = CTExpr "std::variant" . map (\(cn, t) -> toCtorT cn t) $ _ictors
-- Do not expand non inductives as they don't need to
expandind CDFunc { .. } = [CDFunc (mkptr _fd) (map mkptr _fargs) _fbody]
    where mkptr CDef { .. } = CDef _nm $ addPtr _ty
expandind o = [o]
