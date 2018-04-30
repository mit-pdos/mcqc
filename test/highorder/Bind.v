Require Extraction.

Set Implicit Arguments.

Inductive proc: Type -> Type :=
| ret: forall T, T -> proc T
| bind: forall T T', proc T -> (T -> proc T') -> proc T'
| print: nat -> proc unit.



Fixpoint f(a: nat) :=
  match a with
    | 0 => ret tt
    | S n => bind (print a) (fun _ => f n)
  end.

Compute f 10.

Extraction Language JSON.
Separate Extraction f.
