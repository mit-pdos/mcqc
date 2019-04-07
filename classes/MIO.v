Require Import Coq.Strings.String.

Set Implicit Arguments.

Module IO.

  Inductive io: Type -> Type :=
  (** Open filepath to nat *)
  | open : string -> io nat
  (** Open TCP port to nat *)
  | socket: nat -> io nat
  (** Read from nat *)
  | read: nat -> nat -> io string
  (** Write to nat *)
  | write: nat -> string -> io unit
  | link : string -> string -> io bool
  | unlink : string -> io unit
  (** Close nat *)
  | close : nat -> io unit
  (** Get random UUID *)
  | getuuid : io string
  (** Get random number *)
  | randnat: io nat
  (** Get PID *)
  | pidfn: io string
  (** Until loop with initial value*)
  | until : forall T, (T -> bool) -> (option T -> io T) -> option T -> io T
  (** Spawn async future *)
  | spawn: forall T, (T -> unit) -> T -> io unit
  (** Print to stdout *)
  | print : string -> io unit
  (** Monad implementation *)
  | ret: forall T, T -> io T
  | bind: forall T T', io T -> (T -> io T') -> io T'.

  Notation "p1 >>= p2" := (bind p1 p2)
                            (at level 60, right associativity).

  Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
                               (at level 60, right associativity).
End IO.

