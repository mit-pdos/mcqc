Require Import List.
Require Import String.
Import ListNotations.
Open Scope string_scope.

Set Implicit Arguments.

Definition Free {T} (t: T) := t.

Inductive Proc: Type -> Type :=
| open : string -> Proc nat
| print : string -> Proc unit
| read: nat -> Proc string
| close : nat -> Proc unit
| ret: forall T, T -> Proc T
| bind: forall T T', Proc T -> (T -> Proc T') -> Proc T'.

Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
  (at level 60, right associativity).

Definition cat (path: string) (fn : string) :=
  f <- open (path ++ "/" ++ fn);
  contents <- read f;
  _ <- close f;
  _ <- print contents;
  ret unit.

Require Extraction.
Extraction Language JSON.
Separate Extraction cat.
