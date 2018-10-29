{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
module PrettyPrinter.Ind where
import Common.Utils
import CIR.Decl
import CIR.Expr
import PrettyPrinter.Expr()
import Data.Text.Prettyprint.Doc
import Data.Text(Text)
import qualified Data.Text as T

{-
// List implementation here
enum ListCtor {
  NIL,
  CONS
};

template<typename T>
struct list {
  enum ListCtor type;

  struct Nil {};
  list():type(NIL) { n = Nil(); };

  struct Cons {
    T h;
    Ptr<list<T>> ts;
    Cons(T a, Ptr<list<T>> b): h(a) { ts = b; };
  };

  list(T a, Ptr<list<T>> l):type(CONS) { c = Cons(a, l); };
  union {
    Nil n;
    Cons c;
  };

  ~list() {}
};
-}

data CField = CF { nm :: Text,  ty :: CType } deriving (Show, Eq)
data CInd =
    CEnum { name :: Text, tags :: [Text] }
    | CStruct { name :: Text, fields :: [CField], subs ::[CInd]  }
    | CUnion { fields :: [CField] }
    deriving (Show, Eq)

instance Pretty CField where
    pretty CF { .. } = pretty ty <+> pretty nm

-- Zip names and types to fields
zipf :: [Text] -> [CType] -> [CField]
zipf = zipWith (\a b -> CF a b)

-- Give an ord of names to types, useful for making constructors
givenm :: Char -> [CType] -> [CField]
givenm c = zipf [T.pack [i] | i <- [c..]]

-- Intermediate representation before printing inductive as tagged-union
elaborate :: CDecl -> [CInd]
elaborate CDInd   { .. } = [cenum, cstruct]
    where cenum = CEnum (_iname `T.append` "Ctor") $ map (T.toUpper . fst) $ _ictors
          mkenumdecl CEnum { .. } = CF "type" $ CTBase name
          cstruct = CStruct _iname [mkenumdecl cenum] $ map mksubstructs _ictors ++ [mkcunion _ictors]
          mksubstructs (nm, ts) = CStruct (T.toTitle nm) (givenm 'a' . map mkptr $ ts) []
          mkcunion ctrs = CUnion $ zipf (map (T.toLower . fst) ctrs) (map (CTBase . T.toTitle . fst) ctrs)
          mkptr t | t == _itype = CTExpr "std::shared_ptr" [t]
                  | otherwise   = t

--     Cons(T a, Ptr<list<T>> b): h(a) { ts = b; };
instance Pretty CInd where
  pretty CEnum   { .. } = "enum" <+> pretty name <+> "{"
          <> line <> (tab . commatize . map pretty $ tags)
          <> line <>"};"
          <> line
  pretty CStruct { fields = [], subs = [], ..} = "struct" <+> pretty name <+> "{};"
  pretty CStruct { subs = [], .. } =  "struct" <+> pretty name <+> "{"
          <> line <> (tab . vcat . map (\x -> pretty x <> ";") $ fields)
          <> line <> (tab . mkctor name $ fields)
          <> line <> "};"
    where mkctor nm flds = pretty nm <> "(" <> (commatize . mkargs $ flds) <> ") {"
            <+> (concatassgn . mkassgn $ flds) <+> "};"
          concatassgn l  = concatWith (\a b -> a <> ";" <+> b) l <> ";"
          mkassgn = zipWith (\a b -> pretty b <+> "=" <+> pretty a) ['m'..] . map nm
          mkargs = zipWith (\a b -> pretty b <+> pretty a) ['m'..] . map ty
  pretty CStruct { .. } = "struct" <+> pretty name <+> "{"
          <> line <> (tab . vcat . map (\x -> pretty x <> ";") $ fields)
          <> line <> (tab . vcat . map pretty $ subs)
          <> line <> "};"
  pretty CUnion  { .. } = "union {"
          <> line <> (tab . vcat . map (\x -> pretty x <> ";") $ fields)
          <> line <> "};"
  pretty e = error $ "Unhandled Inductive type" ++ show e
