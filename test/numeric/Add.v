(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Add.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp

    CPP: #include "nat.hpp"
    CPP: nat add(nat a, nat b)
    CPP: return match{{.*}}a{{.*}}
    CPP: return b;
    CPP: return match{{.*}}b{{.*}}
    CPP: return a;
    CPP: return succ(succ(add(aM, bM)));
*)

Fixpoint add (a b: nat) :=
  match a with
    | 0    => b
    | S a' =>
       match b with
         | 0    => a
         | S b' => S (S (add a' b'))
       end
  end.

Require Extraction.
Extraction Language JSON.
Separate Extraction add.
