Require Extraction.

Fixpoint fib(n: nat) :=
  match n with
    | 0 => 1
    | 1 => 1
    | S(S m as sm) => fib(m) + fib(sm)
  end.

Extraction Language JSON.
Separate Extraction fib.
