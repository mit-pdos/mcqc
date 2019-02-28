(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %mcqc Curry.json -o %t.cpp
    RUN: %FC %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp

    CPP: #include "nat.hpp"
    CPP: nat add1(nat n)
    CPP: return add((nat)1, n)
}

*)
Definition add1 (n: nat) := Nat.add 1 n.

Require Extraction.
Extraction Language JSON.
Separate Extraction add1.
