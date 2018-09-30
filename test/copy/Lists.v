(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Lists.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp

    CPP: #include "copy.hpp"
    CPP: #include "list.hpp"
    CPP: #include "nat.hpp"
    
    CPP: list<nat> dlists
    CPP: return app(copy(l1), app(l2, l1));

    CPP: list<nat> ddlist
    CPP: return app(cons(copy(h), copy(a)), app(cons(copy(h), copy(a)), cons(h, a)))

    CPP: list<nat> fdlist
    CPP: return match(copy(a),
    CPP: return app(a, ts);
*)
Require MList.
Import MList.List.
Require Import Coq.Lists.List.
Import ListNotations.
Local Open Scope list_scope.

Definition dlists (l1 l2: list nat) := l1 ++ l2 ++ l1.

Definition ddlist (h: nat) (a : list nat) := (cons h a) ++ ((cons h a) ++ (cons h a)).


Fixpoint fdlist (a : list nat) :=
  match a with
    | [] => []
    | h :: ts => a ++ ts
  end.

Require Extraction.
Extraction Language JSON.
Separate Extraction fdlist dlists ddlist.