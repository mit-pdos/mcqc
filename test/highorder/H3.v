(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq H3.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp

    CPP: #include "nat.hpp"
    CPP: template<{{typename|class}} [[TF:.?]]>
    CPP: list<nat> mapOnEvens([[TF]] f, list<nat> l)
*)

Require Export Coq.Lists.List.
Import ListNotations.

Fixpoint isEven(n: nat) :=
  match n with
    | 0 => true
    | 1 => false
    | S(S m as sm) => isEven m
  end.

Fixpoint mapOnEvensM (f : nat -> nat) (n: nat) (l: list nat) : list nat :=
  match l with
    | [] => []
    | h::ts =>
      match isEven n with
      | true => (f h) :: (mapOnEvensM f (n-1) ts)
      | false => h :: (mapOnEvensM f (n-1) ts)
      end
  end.

Definition mapOnEvens (f : nat -> nat) (l : list nat) : list nat :=
  let len := length l in
  match len with
    | 0 => []
    | S n => mapOnEvensM f n l
  end.

Require Extraction.
Extraction Language JSON.
Separate Extraction mapOnEvens.
