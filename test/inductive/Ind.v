Inductive List {T} :=
  | NULL: @List T
  | CONS: T -> @List T -> @List T.

Set Implicit Arguments.
Compute NULL. 
Compute CONS 3 (CONS 2 (CONS 1 NULL)).

Require Extraction.
Extraction Language JSON.
Separate Extraction List.
