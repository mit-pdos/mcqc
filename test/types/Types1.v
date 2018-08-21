(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Types1.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp
    CPP: #include "list.hpp"
    CPP: #include "nat.hpp"
    CPP: using pathname = List<Nat>;
*)

Definition pathname := list nat.

Require Extraction.
Extraction Language JSON.
Separate Extraction pathname.
