(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %mcqc Rev.json -o %t.cpp
    RUN: %FC %s -check-prefix=CPP < %t.cpp
    RUN: %clang %t.cpp

    CPP: #include "variant.hpp"
    CPP: template<{{typename|class}} T>
    CPP: std::shared_ptr<list<T>> rev(std::shared_ptr<list<T>> l)
    CPP: return match{{.*}}l{{.*}}
    CPP: () { return coq_nil<T>(); }
    CPP: (auto h, auto ts) { return app<T>(rev<T>(ts), coq_cons<T>(h, coq_nil<T>())); });
*)
Add LoadPath "../../classes".
Require Import Coq.Lists.List.
Import ListNotations.

Require MProc.
Require MShow.
Import MProc.Proc.
Import MShow.Show.

Set Implicit Arguments.

Fixpoint rev {T} (l : list T) : list T :=
  match l with
    | [] => []
    | h :: ts => rev(ts) ++ [h]
  end.

Fixpoint series (n: nat) :=
  match n with
    | 0 => []
    | S m => n :: series m
  end.

Require Import Coq.Strings.String.
Local Open Scope string_scope.

Definition main :=
  _ <- print "A list";
  _ <- print (show (series 1000));
  _ <- print "Reversed";
  print (show (rev (series 1000))).


Require Extraction.
Extraction Language JSON.
Separate Extraction main.
