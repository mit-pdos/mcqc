(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %mcqc Fib.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp
    RUN: %clang %t.cpp -emit-llvm -g -S -o %t.ll %s
    RUN: FileCheck %s -check-prefix=LLVM < %t.ll

    CPP: #include "nat.hpp"
    CPP: nat fib(nat n)
    CPP: return match{{.*}}n{{.*}}
    CPP: return (nat)1;
    CPP: return match{{.*}}sm{{.*}}
    CPP: return (nat)1;
    CPP: add{{.*}}fib(m){{.*}}fib(sm)

    LLVM: define i32 @{{.*}}fib{{.*}}
    LLVM: icmp eq i32 [[NN:%[0-9]+]], 0
    LLVM: [[SM:%[0-9]+]] = add i32 [[NN]], -1
    LLVM: icmp eq i32 [[SM]], 0
    LLVM: [[MM:%[0-9]+]] = add i32 [[NN]], -2
    LLVM: {{.*}} call {{.*}}i32 @{{.*}}fib{{.*}}[[MM]]
    LLVM: {{.*}} call {{.*}}i32 @{{.*}}fib{{.*}}[[SM]]
*)
Require Import Coq.Init.Nat.

Fixpoint fib(n: nat) :=
  match n with
    | 0 => 1
    | S sm =>
      match sm with
        | 0 => 1
        | S m => (fib m) + (fib sm)
      end
  end.

Require Extraction.
Extraction Language Haskell.
Extract Inductive nat => "Prelude.Int" [ "0" "Prelude.succ" ]
  "(\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))".
Extract Constant add => "(\a b -> if (a Prelude.> (2 Prelude.^ 32 Prelude.- 1)) Prelude.|| (b Prelude.> 2 Prelude.^ 32 Prelude.- 1) then 0 else (a Prelude.+ b))".
Extract Constant mul => "(Prelude.*)".
Extract Inductive bool => "Prelude.Bool" [ "Prelude.True" "Prelude.False" ].
Separate Extraction mul fib.

