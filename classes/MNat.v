(** Matches include/Nat.hpp *)
Require Coq.Init.Nat.

Module MNat.
  Class NativeNat A: Type :=
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

  Instance native: NativeNat nat :=
    {
      succ := S;
      pred := Nat.pred;
      even := Nat.even;
      odd := Nat.odd;
      add := Nat.add;
      sub := Nat.sub;
      mul := Nat.mul;
      div := Nat.div;
      mod := Nat.modulo;
      pow := Nat.pow;
      eqb := Nat.eqb;
      leb := Nat.leb;
      ltb := Nat.ltb;
    }.

  Infix "<=?" := leb (at level 70).
  Infix "<?" := ltb (at level 70).
  Infix "^" := Nat.pow.
  Infix "/" := Nat.div.
  Infix "mod" := Nat.modulo (at level 40, no associativity).

End MNat.
