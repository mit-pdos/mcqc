(** Matches include/string.hpp *)

Require Import Coq.Strings.String.
Require Import Coq.Strings.Ascii.

Module String.
  Definition string := Coq.Strings.String.string.
  Class NativeString String :=
  {
    append:    String -> String -> String;
    get:       nat -> String -> option ascii;
    substring: nat -> nat -> String -> String;
    prefix:    String -> String -> bool;
    size:      String -> nat;
  }.

  Instance nativeString : NativeString string :=
  {
    append := Coq.Strings.String.append;
    get    := Coq.Strings.String.get;
    substring := Coq.Strings.String.substring;
    prefix := Coq.Strings.String.prefix;
    size   := Coq.Strings.String.length;
  }.

  Notation "x ++ y" := (String.append x y) (right associativity, at level 60) : string_scope.
  Notation "x !! n" := (String.get n x) (left associativity, at level 40) : string_scope.
End String.

Require Extraction.
Extraction Language JSON.
Separate Extraction String.nativeString.
