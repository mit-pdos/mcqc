(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Types3.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp

    CPP: #include "nat.hpp"
    CPP: using fn = nat;
    CPP: using pathname = list<list<list<fn>>>;
*)

Definition fn := nat.
Definition pathname := list (list (list fn)).

Require Extraction.
Extraction Language JSON.
Separate Extraction pathname.
