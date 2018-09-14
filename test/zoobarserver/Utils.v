Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Coq.Init.Nat.


(** String equality *)
Module Utils.
  Fixpoint streq (s1 s2: string) :=
    match s1, s2 with
    | EmptyString, EmptyString => true
    | String c1 ts1, String c2 ts2 => andb (nat_of_ascii c1 =? nat_of_ascii c2) (streq ts1 ts2)
    | _, _ => false
    end.
End Utils.