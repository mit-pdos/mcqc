Require Import Coq.Strings.String.
Require Import Coq.Lists.List.

Set Implicit Arguments.

Module Proc.
  Definition Fd := nat.
  Inductive Proc: Type -> Type :=
  (** Open filepath to Fd *)
  | open : string -> Proc Fd
  (** Open TCP port to Fd *)
  | sopen: nat -> Proc Fd
  (** Read from Fd *)
  | read: Fd -> nat -> Proc string
  (** Close Fd *)
  | close : Fd -> Proc unit
  (** Get random number *)
  | random: Proc nat
  (** Until loop with initial value*)
  | until : forall T, (T -> bool) -> (option T -> Proc T) -> option T -> Proc T
  (** Spawn async future *)
  | spawn: forall T, (T -> unit) -> T -> Proc unit
  (** Print to stdout *)
  | print : string -> Proc unit
  (** Monad implementation *)
  | ret: forall T, T -> Proc T
  | bind: forall T T', Proc T -> (T -> Proc T') -> Proc T'.

  Notation "x <- p1 ; p2" := (Proc.bind p1 (fun x => p2))
                               (at level 60, right associativity) : proc_scope.
End Proc.

