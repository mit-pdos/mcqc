(** Matches include/List.hpp *)
Require Coq.Lists.List.

Class NativeList (A : Type) :=
  {
    cons:   list A -> A -> list A;
    dcons:  list A -> A -> list A;
    head:   list A -> A;
    tail:   list A -> list A;
    dtail:  list A -> list A;
    dappd:  list A -> list A -> list A;
    dapp:   list A -> list A -> list A;
    appd:   list A -> list A -> list A;
    app:    list A -> list A -> list A;
    empty:  list A -> bool;
    has:    list A -> bool;
    length: list A -> nat;
  }.

Instance nativeListNat: NativeList nat :=
  {
    cons  := (fun l n => List.cons n l);
    dcons := (fun l n => List.cons n l);
    (** WIP *)
  }.

