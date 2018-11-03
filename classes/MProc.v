Require MString.
Import MString.String.
Require MList.
Import MList.List.

Set Implicit Arguments.

Module Proc.
  Definition fd := nat.
  Definition filename := string.
  Definition pathname := list filename.

  Inductive proc: Type -> Type :=
  (** Open filepath to fd *)
  | open : pathname -> proc fd
  (** Open TCP port to fd *)
  | socket: nat -> proc fd
  (** Read from fd *)
  | read: fd -> nat -> proc string
  (** Write to fd *)
  | write: fd -> string -> proc unit
  | link : pathname -> pathname -> proc bool
  | unlink : pathname -> proc unit
  (** Close fd *)
  | close : fd -> proc unit
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

