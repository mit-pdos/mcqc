(** Matches include/Array.hpp *)

Set Implicit Arguments.

From Mcqc Require Import MIO.
Import MIO.IO.


Module Array.

  Inductive Array: Type -> Type :=
    | get: forall T, Array T -> nat -> Array T
    | put: forall T, Array T -> nat -> T -> Array T
    | empty: forall T,  nat -> T -> Array T
    (** Monad implementation *)
    | ret: forall T, T -> Array T
    | bind: forall T T', Array T -> (T -> Array T') -> Array T'.

  Parameter runArray: forall T,  Array T -> io T.

  (** Define some Arrays *)
  Compute let zero := empty 4 0 in put zero 2 1.

End Array.

