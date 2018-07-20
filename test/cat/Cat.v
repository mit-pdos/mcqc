Require Import List.
Require Import String.
Import ListNotations.
Open Scope string_scope.

Set Implicit Arguments.

Definition pathname := string.
Definition data := string.
Definition fd := nat.
Inductive proc: Type -> Type :=
| open : pathname -> proc fd
| print : data -> proc unit
| read: fd -> proc data
| close : fd -> proc unit
| ret: forall T, T -> proc T
| bind: forall T T', proc T -> (T -> proc T') -> proc T'.

Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
  (at level 60, right associativity).

Definition cat (path: pathname) (fn : string) :=
  f <- open (path ++ fn);
  contents <- read f;
  _ <- close f;
  _ <- print contents;
  ret unit.

Definition Free {T} (t: T) := t.

Fixpoint feven (b : nat) : bool :=
  match Free(b) with
    | 0 => true
    | S 0 => false
    | S (S sm) => andb true (feven sm)
  end.

Check feven.

Require Extraction.
Extraction Language JSON.
Separate Extraction feven.
