(** Matches include/string.hpp *)

Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.
Require Import Coq.Lists.List.

(** Define split of strings to list<string> *)
Module _Private.
  Fixpoint list_of_string (s : string) : list ascii :=
    match s with
    | EmptyString => nil
    | String c s => cons c (list_of_string s)
    end.

  Fixpoint string_of_list (xs : list ascii) : string :=
    fold_right String EmptyString xs.

  Local Open Scope bool_scope.

  Fixpoint ascii_eq (a b: ascii) : bool :=
    match a with
    | Ascii a0 a1 a2 a3 a4 a5 a6 a7 =>
      match b with
      | Ascii b0 b1 b2 b3 b4 b5 b6 b7 =>
        Bool.eqb a0 b0 &&
                 Bool.eqb a1 b1 &&
                 Bool.eqb a2 b2 &&
                 Bool.eqb a3 b3 &&
                 Bool.eqb a4 b4 &&
                 Bool.eqb a5 b5 &&
                 Bool.eqb a6 b6 &&
                 Bool.eqb a7 b7
      end
    end.
  Local Close Scope bool_scope.
  Import ListNotations.
  Fixpoint tokenize_helper (cls : bool) (cmp: ascii -> bool) (acc xs : list ascii)
    : list (list ascii) :=
    let tk := match acc with [] => [] | _::_ => [rev acc] end in
    match xs with
    | [] => tk
    | (x::xs') =>
      match cls, cmp x, x with
      | _, true, _    =>
        tk ++ (tokenize_helper true cmp [] xs')
      | false,false,x  =>
        tokenize_helper false cmp (x::acc) xs'
      | _,tp,x         =>
        tk ++ (tokenize_helper tp cmp [x] xs')
      end
    end %char.


  Definition split (s : string) (sep: ascii) : list string :=
    map string_of_list (tokenize_helper true (fun c => ascii_eq c sep) [] (list_of_string s)).

End _Private.


Module String.
  Definition string := Coq.Strings.String.string.
  Class NativeString String :=
  {
    append:    String -> String -> String;
    get:       nat -> String -> option ascii;
    substring: nat -> nat -> String -> String;
    concat:    String -> list String -> String;
    split:     String -> ascii -> list String;
    prefix:    String -> String -> bool;
    length:    String -> nat;
  }.

  Instance nativeString : NativeString string :=
  {
    append := Coq.Strings.String.append;
    get    := Coq.Strings.String.get;
    substring := Coq.Strings.String.substring;
    concat := Coq.Strings.String.concat;
    split  := _Private.split;
    prefix := Coq.Strings.String.prefix;
    length := Coq.Strings.String.length;
  }.

  Notation "x ++ y" := (String.append x y) (right associativity, at level 60) : string_scope.
  Notation "x !! n" := (String.get n x) (left associativity, at level 40) : string_scope.
End String.

Require Extraction.
Extraction Language JSON.
Separate Extraction String.nativeString.
