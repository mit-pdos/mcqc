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

{-
struct Nil {};

template<typename T>
struct Cons {
    T a;
    Ptr<std::variant<Nil, Cons<T>>> b;
    Cons(T a, Ptr<std::variant<Nil, Cons<T>>> b) { this->a = a; this->b = b; };
};

template<typename T>
using list = std::variant<Nil, Cons<T>>;

// Constructor functions
template<typename T>
Ptr<list<T>> nil() {
    return std::make_shared<list<T>>(Nil());
}

template<typename T>
Ptr<list<T>> cons(T a, Ptr<list<T>> b) {
    return std::make_shared<list<T>>(Cons(a,b));
}

template<typename T, typename Func, typename Func2>
auto match(std::shared_ptr<list<T>> self, Func f, Func2 g) {
    return std::visit(overloaded {
            [&](Nil a) { return f(); },
            [&](Cons<T> b) { return g(b.a, b.b); }
           }, *l);
}
-}

-- Make a match statement for unfolding the Inductive type
mkMatch :: CDef -> [(Text, CType)] -> CDecl
mkMatch CDef { .. } ctors =
    CDFunc matchdef (matchedobj:fdefs) $ CExprCall "return" [
       CExprCall "std::visit" []
    ]
    where matchedobj = CDef "self" $ CTPtr _ty
          matchdef   = CDef "match" CTAuto
          fdefs      = givenm 'f' [CTFree (i+1) | i <- [getMaxVaridx _ty..length ctors]]

-- Make a struct for each Coq inductive constructor with that name
mkCtorStruct :: (Text, CType) -> CDecl
mkCtorStruct (name, CTFunc { .. }) = CDStruct name (givenm 'a' _fins)
mkCtorStruct (name, o) = error $ "Cannot export constructor" ++ show name ++ " of " ++ show o

-- Make C++ variant an alias for Coq inductive type
mkIndAlias :: CDef -> [(Text, CType)] -> CDecl
mkIndAlias CDef { .. } = CDType . CDef _nm . CTExpr "std::variant" . map mkTyp
    where mkTyp (nm, CTFunc { .. }) = let freedom = maximum $ 0:map getMaxVaridx _fins in
            if freedom > 0
            then CTExpr nm [CTFree freedom]
            else CTBase nm

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
expandind CDInd { .. } = map mkCtorStruct _ictors ++ [mkIndAlias _id _ictors] ++ map (mkCtorFunc _id) _ictors ++ [mkMatch _id _ictors]
-- Do not expand non inductives as they don't need to
expandind o = [o]

