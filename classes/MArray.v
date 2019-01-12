(** Matches include/List.hpp *)

Set Implicit Arguments.

Module Array.

  Inductive array: Type -> Type :=
    | get: forall T, array T -> nat -> array T
    | put: forall T, array T -> nat -> T -> array T
    | empty: forall T,  nat -> T -> array T
    (** Monad implementation *)
    | ret: forall T, T -> array T
    | bind: forall T T', array T -> (T -> array T') -> array T'.

  Class NativeArray A List :=
  {
    hget:    nat -> List -> A;
    hput:    nat -> A -> List -> List;
    hempty:  nat -> A -> List;
  }.

  Parameter Array: Type -> Type.

  Instance nativeArray {T}: NativeArray T (Array T). Admitted.                                   

  (** Define some arrays *)
  Definition m := let zero := empty 4 0 in put zero 2 1.
  Definition h := let zero := hempty 4 0 in hput 2 1 zero.

  Compute m.
  Compute h.

  Theorem equalityBySameness: h = h.
    Proof.
      unfold h.
      assert(m = m). unfold m.
      reflexivity. reflexivity.
    Qed.

End Array.

Require Extraction.
Extraction Language JSON.
Separate Extraction Array.m Array.h. 

