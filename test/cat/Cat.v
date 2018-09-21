(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Cat.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp
    RUN: %clang %t.cpp -emit-llvm -g -S -o %t.ll %s

    CPP: #include "nat.hpp"
    CPP: #include "optional.hpp"
    CPP: #include "proc.hpp"
    CPP: #include "string.hpp"
    CPP: #include "tuple.hpp"
*)
Require MNat.
Require MList.
Require MProc.
Require MString.
Import MNat.Nat.
Import MList.List.
Import MProc.Proc.
Import MString.String.

Local Open Scope string_scope.
Local Open Scope proc_scope.

Definition read_loop (f: Fd) :=
  tup <- until
      (fun prev => (snd prev) =? 0)
      (fun prev => s <- read f 4096;
                  ret match prev with
                      | Some (sp, _) => ((sp ++ s), length s)
                      | None => (s, length s)
                      end
      )
      (Some ("", 4096));
    ret (fst tup).

Definition cat (path: string) (fn : string) :=
  f <- open (path ++ "/" ++ fn);
  contents <- read_loop f;
  _ <- close f;
  _ <- prints contents;
  ret unit.

Require Extraction.
Extraction Language JSON.
Separate Extraction cat.

