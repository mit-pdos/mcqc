(**
  RUN: coqc %s
  RUN: machcoq Fib.json
*)

Require Extraction.

Fixpoint fib(n: nat) :=
(** CHECK: match *)
  match n with
    | 0 => 1
    | 1 => 1
    | S(S m as sm) => fib(m) + fib(sm)
  end.

Extraction Language Haskell.
Extract Inductive nat => "Prelude.Int" [ "0" "Prelude.succ" ]
  "(\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))".
Separate Extraction fib.
