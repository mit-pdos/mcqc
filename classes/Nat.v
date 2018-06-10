(** Matches include/Nat.hpp *)
Require Coq.Init.Nat.

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
  }.

Instance nativeNat: NativeNat nat :=
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
  }.

