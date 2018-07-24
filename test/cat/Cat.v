Require Import List.
Require Import String.
Import ListNotations.
Open Scope string_scope.

Set Implicit Arguments.

Definition Free {T} (t: T) := t.

Inductive proc: Type -> Type :=
| open : string -> proc nat
| print : string -> proc unit
| read: nat -> proc string
| close : nat -> proc unit
| ret: forall T, T -> proc T
| bind: forall T T', proc T -> (T -> proc T') -> proc T'.

Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
  (at level 60, right associativity).

Definition cat (path: string) (fn : string) :=
  f <- open (path ++ fn);
  contents <- read f;
  _ <- close f;
  _ <- print contents;
  ret unit.

Require Extraction.
Extraction Language JSON.
Separate Extraction cat.
