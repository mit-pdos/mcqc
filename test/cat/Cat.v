(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %machcoq Cat.json -o %t.cpp
    RUN: FileCheck %s -check-prefix=CPP < %t.cpp

    CPP: #include "nat.hpp"
    CPP: #include "option.hpp"
    CPP: #include "proc.hpp"
    CPP: #include "string.hpp"
    CPP: #include "tuple.hpp"
    CPP: read_loop(fd f)
    CPP: cat(pathname path, string fn)
*)
Require MNat.
Require MList.
Require MProc.
Require MString.
Require MShow.
Import MNat.Nat.
Import MList.List.
Import MProc.Proc.
Import MString.String.
Import MShow.Show.

Local Open Scope string_scope.
Local Open Scope proc_scope.

Definition read_loop (f: fd) :=
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

Local Close Scope string_scope.
Require Import Coq.Lists.List.
Import ListNotations.
Definition cat (path: pathname) (fn : string) :=
  f <- open (path ++ [fn]);
  contents <- read_loop f;
  _ <- close f;
  _ <- print (show contents);
  ret unit.

Require Extraction.
Extraction Language JSON.
Separate Extraction cat.

