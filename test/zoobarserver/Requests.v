(**
    RUN: true
*)
Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Coq.Arith.Arith.
Require Import Coq.Arith.EqNat.
Require Import Coq.Lists.List.
Require Import Coq.Init.Nat.

Require Import NatSerializer.

Import ListNotations.

(* ####################################################### *)
(** ** Lexical Analysis *)
Definition isWhite (c : ascii) : bool :=
  let n := nat_of_ascii c in
  orb (orb (n =? 32)   (* space *)
           (n =? 9))   (* tab *)
      (orb (n =? 10)   (* linefeed *)
           (n =? 13)). (* Carriage return. *)

Inductive chartype := white | other.

Definition classifyChar (c : ascii) : chartype :=
  if isWhite c then
    white
  else
    other.

Fixpoint list_of_string (s : string) : list ascii :=
  match s with
  | EmptyString => []  | String c s => c :: (list_of_string s)
  end.

Fixpoint string_of_list (xs : list ascii) : string :=
  fold_right String EmptyString xs.

Definition token := string.

Fixpoint tokenize_helper (cls : chartype) (acc xs : list ascii)
                       : list (list ascii) :=
  let tk := match acc with [] => [] | _::_ => [rev acc] end in
  match xs with
  | [] => tk
  | (x::xs') =>
    match cls, classifyChar x, x with
    | _, white, _    =>
      tk ++ (tokenize_helper white [] xs')
    | other,other,x  =>
      tokenize_helper other (x::acc) xs'
    | _,tp,x         =>
      tk ++ (tokenize_helper tp [x] xs')
    end
  end %char.

Definition tokenize (s : string) : list string :=
  map string_of_list (tokenize_helper white [] (list_of_string s)).

Local Open Scope string_scope.
Fixpoint serialize_helper (args: list string) :=
  match args with
    | [] => ""
    | lst :: [] => lst
    | h :: ts => h ++ " " ++ (serialize_helper ts)
  end.

Definition serialize (args: list string) :=
  "(" ++ serialize_helper args ++ ")".

(** Public interface to making and parsing zoobar requests *)
Module Requests.
  Definition reqmaker(from to: string) (amount: nat) :=
    serialize ([from; to; NatSerializer.itoa amount]).

  Local Open Scope char_scope.
  Definition reqparser (req: string) : option (string * string * nat) :=
    let reqlen := String.length req in
    match (get 0 req), (get (reqlen - 1) req) with
    | Some "(", Some ")" =>
      match tokenize (substring 1 (reqlen - 2) req) with
      | from::to::zoostr::[] =>
        match NatSerializer.atoi zoostr with
        | Some zoobars => Some (from, to, zoobars)
        | None => None
        end
      | _ => None
      end
    | _, _ => None
    end.
  Close Scope char_scope.
End Requests.

Compute Requests.reqmaker "foo" "bar" 3.
Compute Requests.reqparser "(foo bar 3)".

