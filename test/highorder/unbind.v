Require Export Coq.Lists.List.
Import ListNotations.

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

Compute f 2.

Definition foo := bind (print 2) 
                       (fun _ => bind (print 1) 
                                   (fun _ => ret tt)).

Definition bar := bind (print 2) 
                       (fun _ => ret tt).


Fixpoint unbind_no_f {T}(b: proc T) :=
  match b with
    | bind a f => (unbind_no_f a) (** How to match on lambda f? *)
    | ret _ => []
    | print n => [print n]
  end.

Compute unbind_no_f(foo).

Fixpoint unbind {T}(b: proc T) :=
  match b with
    | bind a (fun _ => f) => (unbind a) ++ (unbind f) (** How to match on lambda f? *)
    | ret _ => []
    | print n => [print n]
  end.

