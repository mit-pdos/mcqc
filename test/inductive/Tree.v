(**
    RUN: %coqc %s
    RUN: %mcqc Tree.json -o %t.cpp
    RUN: %FC %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp
    RUN: %clean

    Library declaration
    CPP: #include "variant.hpp"
    CPP: using namespace Variant;

    Base types
    CPP: template<class [[TT:.?]]>
    CPP: struct Coq_leaf {
    CPP:    [[TT]] a;
    CPP:    Coq_leaf([[TT]] a) {
    CPP:      this->a = a;

    CPP: template<class [[TT]]>
    CPP: struct Coq_branch {
    CPP: [[TT]] a;
    CPP:    std::shared_ptr<std::variant<Coq_leaf<[[TT]]>, Coq_branch<[[TT]]>>> b;
    CPP:    std::shared_ptr<std::variant<Coq_leaf<[[TT]]>, Coq_branch<[[TT]]>>> c;
    CPP:    Coq_branch([[TT]] a, std::shared_ptr<std::variant<Coq_leaf<[[TT]]>, Coq_branch<[[TT]]>>> b, std::shared_ptr<std::variant<Coq_leaf<[[TT]]>, Coq_branch<[[TT]]>>> c) {
    CPP:        this->a = a;
    CPP:        this->b = b;
    CPP:        this->c = c;

    Variant type alias
    CPP: template<class [[TT]]>
    CPP: using tree = std::variant<Coq_leaf<[[TT]]>, Coq_branch<[[TT]]>>;

    Smart constructors
    CPP: template<class [[TT]]>
    CPP: std::shared_ptr<tree<[[TT]]>> coq_leaf([[TT]] a) {
    CPP:   return std::make_shared<tree<[[TT]]>>(Coq_leaf<[[TT]]>(a));

    CPP: template<class [[TT]]>
    CPP: std::shared_ptr<tree<[[TT]]>> coq_branch([[TT]] a, std::shared_ptr<tree<[[TT]]>> b, std::shared_ptr<tree<[[TT]]>> c) {
    CPP:   return std::make_shared<tree<[[TT]]>>(Coq_branch<[[TT]]>(a, b, c));

    Pattern match
    CPP: template<class [[TT]], class [[TF:.?]], class [[TG:.?]]>
    CPP: auto match(std::shared_ptr<tree<[[TT]]>> self, [[TF]] f, [[TG]] g) {
    CPP:   return gmatch(self, [=](Coq_leaf<[[TT]]> _) { return f(_.a); }, [=](Coq_branch<[[TT]]> _) { return g(_.a, _.b, _.c); });
*)
Add Rec LoadPath "../../classes" as Mcqc.
From Mcqc Require MIO.
Import MIO.IO.

From Mcqc Require MShow.
Import MShow.Show.

Require Import Coq.Lists.List.
Import ListNotations.

Set Implicit Arguments.

Inductive tree (T: Type) :=
  | leaf : T -> tree T
  | branch : T -> tree T -> tree T -> tree T.

Fixpoint inorder {T} (t: tree T) : list T :=
  match t with
    | branch t l r => inorder l ++ [t] ++ inorder r
    | leaf t => [t]
  end.

Compute inorder (branch 3 (leaf 2) (leaf 1)).

Definition main := print (show (inorder (branch 2 (leaf 1) (leaf 3)))).

Require Extraction.
Extraction Language JSON.
Separate Extraction main.
