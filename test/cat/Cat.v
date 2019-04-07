(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %mcqc Cat.json -o %t.cpp
    RUN: %FC %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp

    CPP: #include "io.hpp"
    CPP: #include "nat.hpp"
    CPP: #include "pair.hpp"
    CPP: #include "show.hpp"
    CPP: #include "string.hpp"
    CPP: read_loop(nat f)
    CPP: cat(string path, string fn)
*)
Add LoadPath "../../classes".
Require MIO.
Import MIO.IO.
Require MShow.
Import MShow.Show.

Require Import Coq.Init.Nat.
Require Import Coq.Strings.String.
Local Open Scope string_scope.
Local Open Scope nat_scope.

Definition read_loop (f: nat) :=
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
  _ <- print (show contents);
  ret unit.

Require Extraction.
Extraction Language JSON.
Separate Extraction cat.

