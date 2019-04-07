(**
   XFAIL: true
*)
Add LoadPath "../../classes".
Require MIO.
Require MShow.
Import MIO.IO.
Import MShow.Show.

Require Import Coq.Lists.List.
Import ListNotations.

Definition main :=
  print (show (map (fun x => x+x) [1;2])).

Require Extraction.
Extraction Language JSON.
Separate Extraction main.



