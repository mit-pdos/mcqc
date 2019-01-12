(**
  RUN: %coqc %s
  RUN: %clean
  RUN: %mcqc Hello.json -o %t.cpp
  RUN: FileCheck %s -check-prefix=CPP < %t.cpp
  RUN: %clang %t.cpp

  CPP: #include "proc.hpp"
  CPP: using namespace Proc;
  CPP: int main()
  CPP: print(string("Hello world"));
  CPP: return 0;
*)
Add LoadPath "../../classes".
Require MProc.
Import MProc.Proc.

Local Open Scope string_scope.
Definition main :=
  print("Hello world").

Require Extraction.
Extraction Language JSON.
Separate Extraction main.
