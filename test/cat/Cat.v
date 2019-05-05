(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %mcqc Cat.json -o %t.cpp
    RUN: %FC %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp

    CPP: #include "io.hpp"
    CPP: #include "nat.hpp"
    CPP: #include "prod.hpp"
    CPP: #include "show.hpp"
    CPP: #include "string.hpp"

    Read a file in a loop
    CPP: io<string> read_loop(nat f)
    CPP: tup = until([=](auto prev) { return snd(prev) == (nat)0; }
    CPP: s = read(f, (nat)4096)
    CPP: return std::make_pair(append(sp, s), length(s));
    CPP: return std::make_pair(s, length(s));
    CPP: return fst<io<string>>(tup);

    Cat body
    CPP: io<void> cat(string path, string fn)
    CPP: f = open(append(path, append(string("/"), fn)));
    CPP: contents = read_loop(f);
    CPP: close(f);
    CPP: print(show(contents));
    CPP: return
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
  ret tt.

Require Extraction.
Extraction Language JSON.
Separate Extraction cat.

