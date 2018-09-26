Require Import Coq.Strings.String.
Require Import Coq.Lists.List.

Set Implicit Arguments.

Module Proc.
  Definition fd := nat.
  Inductive proc: Type -> Type :=
  (** Open filepath to fd *)
  | open : string -> proc fd
  (** Open TCP port to fd *)
  | sopen: nat -> proc fd
  (** Read from fd *)
  | read: fd -> nat -> proc string
  (** Write to fd *)
  | write: fd -> string -> proc unit
  (** Close fd *)
  | close : fd -> proc unit
  (** Get random number *)
  | nrand: proc nat
  (** Until loop with initial value*)
  | until : forall T, (T -> bool) -> (option T -> proc T) -> option T -> proc T
  (** Spawn async future *)
  | spawn: forall T, (T -> unit) -> T -> proc unit
  (** Print to stdout *)
  | print : string -> proc unit
  (** Monad implementation *)
  | ret: forall T, T -> proc T
  | bind: forall T T', proc T -> (T -> proc T') -> proc T'.

  Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
                               (at level 60, right associativity) : proc_scope.
End Proc.

