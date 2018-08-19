Require Import List.
Import ListNotations.

Set Implicit Arguments.

Parameter filename : Type.
Parameter fd : Type.
Parameter data : Type.
Definition pathname := list filename.

Inductive proc: Type -> Type :=
| open : pathname -> proc fd
| write : fd -> data -> proc unit
| close : fd -> proc unit
| pidfn : proc filename
| random : proc filename
| link : pathname -> pathname -> proc bool
| unlink : pathname -> proc unit
| ret: forall T, T -> proc T
| bind: forall T T', proc T -> (T -> proc T') -> proc T'
| whilefalse : proc bool -> proc unit.

Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
  (at level 60, right associativity).

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
