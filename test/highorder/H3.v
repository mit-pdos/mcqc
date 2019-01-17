(**
    XFAIL: true
    RUN: %coqc %s
    RUN: %clean
    RUN: %mcqc H3.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp

    CPP: #include "nat.hpp"
    CPP: bool isEven(nat n)
    CPP: return match(n,
    CPP: return true
    CPP: return match(sm,
    CPP: return false
    CPP: return isEven(m)

    CPP: template<{{typename|class}} [[TF:.?]]>
    CPP: std::shared_ptr<list<nat>> mapOnEvensM([[TF]] f, nat n, std::shared_ptr<list<nat>> l)
    CPP: return match(l,
    CPP: return coq_nil<nat>()
    CPP: return match(isEven(n),
    CPP: return coq_cons<nat>(f(h), mapOnEvensM(f, {{.*}}, ts
    CPP: return coq_cons<nat>(h, mapOnEvensM(f, {{.*}}, ts

    CPP: template<{{typename|class}} [[TF:.?]]>
    CPP: list<nat> mapOnEvens([[TF]] f, list<nat> l)
    CPP: len = length(l)
    CPP: return match(len,
    CPP: return list<nat>{}
    CPP: return mapOnEvensM(f, n, l)
*)
Add LoadPath "../../classes".
Require MProc.
Import MProc.Proc.
Require MShow.
Import MShow.Show.
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

Require MString.
Import MString.String.
Local Open Scope string_scope.

Fixpoint showl {T} {showT: Show T} (l : list T) : string :=
  "[" ++ match l with
         | [] => ""
         | a::[] => (show a)
         | a::ts => (show a) ++ ", " ++ (showl ts)
         end ++ "]".

Instance showList {T} {showT: Show T} : Show (list T) :=
  {
    show := showl
  }.


Definition main := print (showl (mapOnEvens (fun x => x + x) [1;2;3;4;5;6])).

Require Extraction.
Extraction Language JSON.
Separate Extraction main.
