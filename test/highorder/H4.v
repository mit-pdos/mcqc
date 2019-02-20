(**
   XFAIL: true
*)
Add LoadPath "../../classes".
Require MProc.
Require MShow.
Import MProc.Proc.
Import MShow.Show.

Require Import Coq.Lists.List.
Import ListNotations.

Definition main :=
  print (show (map (fun x => x+x) [1;2])).

Require Extraction.
Extraction Language JSON.
Separate Extraction main.



