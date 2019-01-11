Require MString.
Import MString.String.

Set Implicit Arguments.

Module Proc.

  Inductive proc: Type -> Type :=
  (** Open filepath to nat *)
  | open : string -> proc nat
  (** Open TCP port to nat *)
  | socket: nat -> proc nat
  (** Read from nat *)
  | read: nat -> nat -> proc string
  (** Write to nat *)
  | write: nat -> string -> proc unit
  | link : string -> string -> proc bool
  | unlink : string -> proc unit
  (** Close nat *)
  | close : nat -> proc unit
  (** Get random UUID *)
  | getuuid : proc string
  (** Get random number *)
  | randnat: proc nat
  (** Get PID *)
  | pidfn: proc string
  (** Until loop with initial value*)
  | until : forall T, (T -> bool) -> (option T -> proc T) -> option T -> proc T
  (** Spawn async future *)
  | spawn: forall T, (T -> unit) -> T -> proc unit
  (** Print to stdout *)
  | print : string -> proc unit
  (** Monad implementation *)
  | ret: forall T, T -> proc T
  | bind: forall T T', proc T -> (T -> proc T') -> proc T'.

  Notation "p1 >>= p2" := (bind p1 p2)
                            (at level 60, right associativity).

  Notation "x <- p1 ; p2" := (bind p1 (fun x => p2))
                               (at level 60, right associativity) : proc_scope.
End Proc.

