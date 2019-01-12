(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %mcqc Rev.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp

    CPP: #include "variant.hpp"
    CPP: template<{{typename|class}} T>
    CPP: std::shared_ptr<list<T>> rev(std::shared_ptr<list<T>> l)
    CPP: return match{{.*}}l{{.*}}
    CPP: () { return coq_nil<T>(); }
    CPP: (auto h, auto ts) { return app<T>(rev<T>(ts), coq_cons<T>(h, coq_nil<T>())); });
*)

Require Import Coq.Lists.List.
Require Extraction.
Import ListNotations.

Set Implicit Arguments.

Fixpoint rev {T} (l : list T) : list T :=
  match l with
    | [] => []
    | h :: ts => rev(ts) ++ [h]
  end.

Extraction Language JSON.
Separate Extraction rev.
