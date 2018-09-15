(**
    RUN: true
*)
Require Import Coq.Strings.String.
Require Import Coq.Lists.List.
Require Import Coq.Init.Nat.
Require Import Coq.Strings.Ascii.

Require Export Requests.
Require Export Utils.

Import ListNotations.

Compute Requests.reqmaker "foo" "bar" 3.
Compute Requests.reqparser "(foo bar 3)".

Definition Fd := nat.
Inductive Proc: Type -> Type :=
| open : string -> Proc Fd
| print : string -> Proc unit
| read: Fd -> nat -> Proc string
| close : Fd -> Proc unit
| until : forall T, (T -> bool) -> (option T -> Proc T) -> option T -> Proc T
| ret: forall T, T -> Proc T
| bind: forall T T', Proc T -> (T -> Proc T') -> Proc T'.

Notation "x <- p1 ; p2" := (bind p1 (fun x => p2 x))
  (at level 60, right associativity).

(** Subtract zoobars helper, check remainder and apply operation to reduce zoobars *)
Fixpoint subE (db befr: list (string * nat)) (user: string) (zb: nat) :=
  match db with
    | [] => ([], false)   (** failure *)
    | (u, dep) :: ts =>
      if Utils.streq u user
      then if zb <=? dep
           then (befr ++ [ (u, dep - zb) ] ++ ts, true) (** success *)
           else (db, false)       (** failure *)
      else subE ts (befr ++ [(u, dep)]) user zb
  end.

(** Add zoobars helper, if subtraction succeeded, add zoobars *)
Fixpoint addM (db befr: list (string * nat)) (user: string) (zb: nat) :=
  match db with
    | [] => []
    | (u, dep) :: ts =>
      if Utils.streq u user
      then befr ++ [ (u, dep + zb) ] ++ ts
      else addM ts (befr ++ [(u, dep)]) user zb
  end.


(** Apply a transfer, from user to user of some zoobars *)
Definition xfer (db: list (string * nat)) (from: string) (to: string) (zb: nat) :=
  match subE db [] from zb with
    | (dbn, true) => addM dbn [] to zb  (** If subtraction returned `ok` *)
    | (_, false) => db               (** Otherwise do nothing *)
  end.

(** High level xfer request handler, RPC endpoint *)
Definition xfer_handler (db: list (string * nat)) (req: string) :=
  match Requests.reqparser req with
    | None => db
    | Some (from, to, zb) => xfer db from to zb
  end.

Open Scope string_scope.
Compute let init := [("alice", 10) ; ("bob", 10); ("charlie", 10)] in
        let nxt := xfer_handler init "(bob alice 5)" in
        xfer_handler nxt "(alice charlie 10)".
