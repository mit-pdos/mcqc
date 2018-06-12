Require Import List.
Import ListNotations.

Set Implicit Arguments.

Parameter filename : Type.
Parameter fd : Type.
Parameter data : Type.
Definition pathname := list filename.

Inductive proc: Type -> Type :=
| open : pathname -> proc fd
| print : data -> proc unit
| read: fd -> nat -> proc data
| close : fd -> proc unit
| ret: forall T, T -> proc T
| bind: forall T T', proc T -> (T -> proc T') -> proc T'.

Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
  (at level 60, right associativity).

Definition cat (path: pathname) (fn : filename) :=
  fd <- open (path ++ [fn]);
  contents <- read fd 1;
  _ <- close fd;
  _ <- print contents;
  ret unit.

Require Extraction.
Extraction Language JSON.
Recursive Extraction cat.
