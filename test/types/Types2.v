(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Types2.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp

    CPP: #include "list.hpp"
    CPP: using filename = auto;
    CPP: using pathname = list<filename>;
*)

Parameter filename : Type.
Definition pathname := list filename.

Require Extraction.
Extraction Language JSON.
Separate Extraction pathname.
