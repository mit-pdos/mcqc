(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %mcqc Types1.json -o %t.cpp
    RUN: %FC %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp

    CPP: #include "nat.hpp"
    CPP: using pathname = list<nat>;
*)

Definition pathname := list nat.

Require Extraction.
Extraction Language JSON.
Separate Extraction pathname.
