Require Import Coq.Lists.List.
Require Extraction.
Import ListNotations.

Set Implicit Arguments.

Fixpoint rev {T} (l : list T) : list T :=
  match l with
    | [] => []
    | h :: ts => rev(ts) ++ [h]
  end.

Extraction Language JSON.
Separate Extraction rev.
