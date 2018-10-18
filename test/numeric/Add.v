Fixpoint add (a b: nat) :=
  match a with
    | 0    => b
    | S a' =>
       match b with
         | 0    => a
         | S b' => S (S 0)
       end
  end.

Require Extraction.
Extraction Language JSON.
Separate Extraction add.
