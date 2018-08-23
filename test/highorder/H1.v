(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq H1.json -o %t.cpp
*)

Fixpoint isEven(n: nat) :=
  match n with
    | 0 => true
    | 1 => false
    | S(S m as sm) => isEven m
  end.

Definition onEvens (f : nat -> nat) (n: nat) :=
  match (isEven n) with
    | true => f n
    | false => n
  end.

Import Coq.Init.Nat. 
Compute onEvens (fun x => div x 2) 10.

Require Extraction.
Extraction Language JSON.
Separate Extraction onEvens.
