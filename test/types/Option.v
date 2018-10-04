(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Option.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp

    CPP: #include "option.hpp"
    CPP: template<{{typename|class}} T>
    CPP: option<T> mksome(T a)
    CPP: return some<T>(a)

*)

Definition mksome {T} (a: T) : option T := Some a.

Require Extraction.
Extraction Language JSON.
Separate Extraction mksome.
