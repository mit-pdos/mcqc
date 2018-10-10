(** Matches include/nat.hpp *)
Require Coq.Init.Nat.

Module Nat.
  Class NativeNat A :=
    {
      succ: A -> A;
      pred: A -> A;
      even: A -> bool;
      odd: A -> bool;
      add: A -> A -> A;
      sub: A -> A -> A;
      mul: A -> A -> A;
      div: A -> A -> A;
      mod: A -> A -> A;
      pow: A -> A -> A;
      eqb: A -> A -> bool;
      leb: A -> A -> bool;
      ltb: A -> A -> bool;
    }.

  Instance nativeNat : NativeNat nat :=
    {
      succ := Coq.Init.Nat.succ;
      pred := Coq.Init.Nat.pred;
      even := Coq.Init.Nat.even;
      odd := Coq.Init.Nat.odd;
      add := Coq.Init.Nat.add;
      sub := Coq.Init.Nat.sub;
      mul := Coq.Init.Nat.mul;
      div := Coq.Init.Nat.div;
      mod := Coq.Init.Nat.modulo;
      pow := Coq.Init.Nat.pow;
      eqb := Coq.Init.Nat.eqb;
      leb := Coq.Init.Nat.leb;
      ltb := Coq.Init.Nat.ltb;
    }.

  Infix "=?" := eqb (at level 70) : nat_scope.
  Infix "<=?" := leb (at level 70) : nat_scope.
  Infix "<?" := ltb (at level 70) : nat_scope.
  Infix "+" := Nat.add : nat_scope.
  Infix "-" := Nat.sub : nat_scope.
  Infix "*" := Nat.mul : nat_scope.
  Infix "/" := Nat.div : nat_scope.
  Infix "^" := Nat.pow : nat_scope.
  Infix "%" := Nat.modulo (at level 40, no associativity) : nat_scope.
End Nat.

Require Extraction.
Extraction Language JSON.
Separate Extraction Nat.nativeNat.
