(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Rev.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp

    CPP: #include "list.{{(h|hpp|cpp)}}"
    CPP: template<typename T>
    CPP: List<T> rev(List<T>& l)
    CPP: return match{{.*}}l{{.*}}
    CPP: () { return List<T>{}; }
    CPP: (auto h, auto ts) { return app{{.*}}rev(ts)
    CPP: List<T>{h}
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
