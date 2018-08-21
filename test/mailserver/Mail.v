(**
    XFAIL
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Mail.json -o %t.cpp
*)	
Require Import List.
Import ListNotations.

Set Implicit Arguments.

Parameter filename : Type.
Definition Fd := nat.
Parameter data : Type.
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
| bind: forall T T', Proc T -> (T -> Proc T') -> Proc T'
| whilefalse : Proc bool -> Proc unit.

Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
  (at level 60, right associativity).

(**
Definition whilefalse (f: bool -> Proc unit) := 
  until id (fun o => match o with
                  | None => f
                  | Some true => f true
                  | _ => tt
                  end) None.
*)

Definition mail_deliver (msg : data) (tmpdir : pathname) (mboxdir : pathname) :=
  tmpfn <- pidfn;
  fd <- open (tmpdir ++ [tmpfn]);
  _ <- write fd msg;
  _ <- close fd;
  whilefalse
    ( mailfn <- random;
      ok <- link (tmpdir ++ [tmpfn]) (mboxdir ++ [mailfn]);
      if ok then
        _ <- unlink (tmpdir ++ [tmpfn]);
        ret true
      else
        ret false ).

Require Extraction.
Extraction Language JSON.
Separate Extraction mail_deliver.
