{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Codegen.Ind where
import CIR.Expr
import CIR.Decl
import Control.Lens
import Common.Utils
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Text as T

-- Take a constructor name and a type, create a constructor type for that name
toCtorT :: Text -> CType -> CType
toCtorT nm CTFunc { .. } =
    if freedom > 0 then CTExpr nm [CTFree freedom] else CTBase nm
    where freedom = maximum $ 0:map getMaxVaridx _fins

-- Make a match statement for unfolding the Inductive type
mkMatch :: CDef -> [(Text, CType)] -> CDecl
mkMatch CDef { .. } ctors =
    CDFunc matchdef (matchedobj:fdefs) $ CExprCall "return" [
       CExprCall "gmatch" $ (CExprVar "self"):(map flams $ zipAdd (map (view nm) fdefs) ctors)
    ]
    where matchedobj                    = CDef "self" $ CTPtr _ty
          matchdef                      = CDef "match" CTAuto
          fdefs                         = givenm 'f' [CTFree (i+1) | i <- [getMaxVaridx _ty..length ctors]]
          flams (f,o, ft@CTFunc { .. }) = CExprLambda [CDef "a" $ toCtorT o ft] $ CExprCall f (dodots $ length _fins)
          dodots n                      = [CExprVar ("a" `T.append` "." `T.append` T.pack [i]) | i <- take n ['a'..]]

-- Make a struct for each Coq inductive constructor with that name
-- Args: constructor: (Text, CType)
--       Struct recursive type (own type unaliased)
mkCtorStruct :: CType -> (Text, CType) -> CDecl
mkCtorStruct unaliasT (name, ft@CTFunc { .. }) = CDStruct name (givenm 'a' . map (mkRecTypes _fret) $ _fins)
    where mkRecTypes t rec | CTPtr t == rec = CTPtr unaliasT | otherwise = rec
mkCtorStruct _ (name, o) = error $ "Cannot export constructor" ++ show name ++ " of " ++ show o

-- Make C++ variant an alias for Coq inductive type
mkIndAlias :: CDef -> [(Text, CType)] -> CDecl
mkIndAlias CDef { .. } = CDType . CDef _nm . CTExpr "std::variant" . map (\(cn, t) -> toCtorT cn t)

-- Make C++ ctor functions for each Coq inductive constructor
mkCtorFunc :: CDef -> (Text, CType) -> CDecl
mkCtorFunc CDef { .. } (ctornm, CTFunc { .. }) =
    CDFunc fd defs $
      CExprCall "return" [    -- return inside the ctor body
        CExprCall sharedptr [ -- wrap in a shared pointer
          CExprCall ctornm . map (CExprVar . view nm) $ defs -- Call struct constructor in mkCDStruct
        ]
      ]
    where fd        = CDef (T.toLower ctornm) (CTPtr _ty)
          defs      = givenm 'a' _fins
          -- TODO: Prints _ty early, make a typed call expression instead
          sharedptr = T.concat ["std::make_shared<", renderStrict . layoutPretty defaultLayoutOptions . pretty $ _ty, ">"]

-- Intermediate representation before printing inductive as tagged-unioan, the order of the list is important
expandind :: CDecl -> [CDecl]
expandind CDInd { .. } = map (mkCtorStruct unaliasedT) _ictors ++ [mkIndAlias _id _ictors] ++ map (mkCtorFunc _id) _ictors ++ [mkMatch _id _ictors]
    where unaliasedT = CTExpr "std::variant" . map (\(cn, t) -> toCtorT cn t) $ _ictors
-- Do not expand non inductives as they don't need to
expandind o = [o]

