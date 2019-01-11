(**
    RUN: %coqc %s
    RUN: %machcoq List.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp
    RUN: %clean

    Library declaration
    CPP: #include "variant.hpp"
    CPP: using namespace Variant;

    Base types
    CPP: template<{{class}} [[TT:.?]]>
    CPP: struct Coq_nil {};
    CPP: template<{{class}} [[TT]]>
    CPP: struct Coq_cons {
    CPP: [[TT]] [[AA:.?]];
    CPP: std::shared_ptr<std::variant<Coq_nil<[[TT]]>, Coq_cons<[[TT]]>>> [[BB:.?]];
    CPP: Coq_cons([[TT]] [[AA]], std::shared_ptr<std::variant<Coq_nil<[[TT]]>, Coq_cons<[[TT]]>>> [[BB]]) {
    CPP: this->[[AA]] = [[AA]];
    CPP: this->[[BB]] = [[BB]];
    CPP: }

    Type alias for sum type
    CPP: template<class [[TT]]>
    CPP: using list = std::variant<Coq_nil<[[TT]]>, Coq_cons<[[TT]]>>

    Coq constructor functions
    CPP: template<class [[TT]]>
    CPP: std::shared_ptr<list<[[TT]]>> coq_nil()
    CPP: return std::make_shared<list<[[TT]]>>(Coq_nil<[[TT]]>());
    CPP: template<class [[TT]]>
    CPP: std::shared_ptr<list<[[TT]]>> coq_cons([[TT]] [[AA]], std::shared_ptr<list<[[TT]]>> [[BB]])
    CPP: return std::make_shared<list<[[TT]]>>(Coq_cons<[[TT]]>([[AA]], [[BB]]));

    Match definition
    CPP: template<class [[TT]], class [[UU:.?]], class [[VV:.?]]>
    CPP: match(std::shared_ptr<list<[[TT]]>> self, [[UU]] f, [[VV]] g)
    CPP: return gmatch(self, [=](Coq_nil<[[TT]]> _) { return f(); }, [=](Coq_cons<[[TT]]> _) { return g(_.[[AA]], _.[[BB]]); });
    CPP: }
*)


Inductive list (A : Type) : Type :=
 | nil : list A
 | cons : A -> list A -> list A.

Require Extraction.
Extraction Language JSON.
Separate Extraction list.
