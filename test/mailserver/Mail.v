(**
    XFAIL
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Mail.json -o %t.cpp
*)
Add LoadPath "../../classes".
Require MList.
Import MList.List.
Require MString.
Import MString.String.
Require MProc.
Import MProc.Proc.
Set Implicit Arguments.

Definition data := string.

Require Import Coq.Lists.List.
Import ListNotations.

Local Close Scope string_scope.
Definition mail_deliver (msg : data) (tmpdir : pathname) (mboxdir : pathname) :=
  pidfn >>=
    (fun tmpfn => open (tmpdir ++ [tmpfn]) >>=
      (fun fd => write fd msg >>=
        (fun _ => close fd >>=
          (fun _ => until (fun _ => false) 
                       (fun _ => 
                         (rands >>= 
                           (fun mailfn => link (tmpdir ++ [tmpfn]) (mboxdir ++ [mailfn]) >>=
                             (fun ok => if ok then
                                        unlink (tmpdir ++ [tmpfn]) >>= (fun _ => ret true)
                                      else
                                        ret false)))) None)))).


Require Extraction.
Extraction Language JSON.
Separate Extraction mail_deliver.
