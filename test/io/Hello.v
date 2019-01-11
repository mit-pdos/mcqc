Add LoadPath "../../classes".
Require MProc.
Import MProc.Proc.

Local Open Scope string_scope.
Definition main := 
  print("Hello world").

Require Extraction.
Extraction Language JSON.
Separate Extraction main.