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

(* Unrolled once *)
Fixpoint f2(a: nat) :=
  match a with
    | 0 => ret tt
    | S n => bind (print a) (fun _ => match n with
                            | 0 => ret tt
                            | S m => bind (print n) (fun _=> f2 m)
                            end)
  end.


Inductive it : Type :=
  | O
  | forall n, Succ n => it.

Class Iterator A : Type :=
  {
    zero: A;
    maxval: A;
    succ : A -> A;
  }.

Instance It : Iterator nat :=
  {
    maxval := 100;
    zero := 0;
    succ := S;
  }.

Definition it := nat.


Compute f2 10.

(* Look for sucessive calls to bind (print succ(a)) *)
(* n = a - 1 -> step = -1 *)

Extraction Language JSON.
Separate Extraction f.
