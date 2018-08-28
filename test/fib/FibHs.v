(**
    RUN: %coqc %s
    RUN: ghc -O3 %p/Main.hs -o %t
*)

Require Extraction.

Fixpoint fib(n: nat) :=
  match n with
    | 0 => 1
    | 1 => 1
    | S(S m as sm) => fib(m) + fib(sm)
  end.

Extraction Language Haskell.
Extract Inductive nat => "Prelude.Int" [ "0" "Prelude.succ" ]
  "(\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))".
Separate Extraction fib.
