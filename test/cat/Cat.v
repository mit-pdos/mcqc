(**
    XFAIL: *
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Cat.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp

    CPP: #include "proc.{{(h|hpp|cpp)}}"
    CPP: #include "string.{{(h|hpp|cpp)}}"
*)

Require Import List.
Require Import String.
Require Import Nat.
Import ListNotations.
Open Scope string_scope.
Open Scope nat_scope.
Set Implicit Arguments.

Definition Fd := nat.

Inductive Proc: Type -> Type :=
| open : string -> Proc Fd
| print : string -> Proc unit
| read: Fd -> nat -> Proc string
| close : Fd -> Proc unit
| until : forall T, (T -> bool) -> (option T -> Proc T) -> option T -> Proc T
| ret: forall T, T -> Proc T
| bind: forall T T', Proc T -> (T -> Proc T') -> Proc T'.

Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
  (at level 60, right associativity).

Definition read_loop (f: Fd) := tup <- until (fun prev => (snd prev) =? 0)
                                      (fun prev => s <- read f 4096;
                                                ret match prev with
                                                       | Some (s', _) => (s' ++ s, length s)
                                                       | None => (s, length s)
                                                       end
                                       )
                                      (Some ("", 4096));
                                  ret (fst tup).


Definition cat (path: string) (fn : string) :=
  f <- open (path ++ "/" ++ fn);
  contents <- read_loop f;
  _ <- close f;
  _ <- print contents;
  ret unit.

Require Extraction.
Extraction Language JSON.
Separate Extraction cat.
