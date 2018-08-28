(**
    XFAIL
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Mail.json -o %t.cpp
*)	
Require Import List.
Require Import String.
Import ListNotations.

Set Implicit Arguments.

Definition filename := string.
Definition Fd := nat.
Definition data := string.
Definition pathname := list filename.

Inductive Proc: Type -> Type :=
| open : pathname -> Proc Fd
| write : Fd -> data -> Proc unit
| close : Fd -> Proc unit
| pidfn : Proc filename
| random : Proc filename
| link : pathname -> pathname -> Proc bool
| unlink : pathname -> Proc unit
| until : forall T, (T -> bool) -> (option T -> Proc T) -> option T -> Proc T
| ret: forall T, T -> Proc T
| bind: forall T T', Proc T -> (T -> Proc T') -> Proc T'.

Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
  (at level 60, right associativity).

Definition mail_deliver (msg : data) (tmpdir : pathname) (mboxdir : pathname) :=
  tmpfn <- pidfn;
  fd <- open (tmpdir ++ [tmpfn]);
  _ <- write fd msg;
  _ <- close fd;
  until (fun _ => false) (fun _ =>
    ( mailfn <- random;
      ok <- link (tmpdir ++ [tmpfn]) (mboxdir ++ [mailfn]);
      if ok then
        _ <- unlink (tmpdir ++ [tmpfn]);
        ret true
      else
        ret false )) None.

Require Extraction.
Extraction Language JSON.
Separate Extraction mail_deliver.
