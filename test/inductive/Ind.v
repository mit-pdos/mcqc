(**
    RUN: %coqc %s
*)

Inductive List {T} :=
  | Nil: @List T
  | Cons: T -> @List T -> @List T.

Require Extraction.
Extraction Language JSON.
Separate Extraction List.
