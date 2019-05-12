(**
    RUN: %coqc %s
    RUN: %clean
    RUN: %mcqc InputOutput.json -o %t.cpp
    RUN: %FC %s -check-prefix=CPP < %t.cpp
    RUN: %clang -c %t.cpp

    CPP: #include "io.hpp"
    CPP: #include "nat.hpp"
    CPP: #include "prod.hpp"
    CPP: #include "string.hpp"

    Read a file in a loop
    CPP: io<string> read_loop(nat f)
    CPP: tup = until([=](auto prev) { return snd(prev) == (nat)0; }
    CPP: s = read(f, (nat)4096)
    CPP: return std::make_pair(append(sp, s), length(s));
    CPP: return std::make_pair(s, length(s));
    CPP: return fst<io<string>>(tup);

    input_output body
    CPP: io<void> input_output(string greeting)
    CPP: print(greeting);
    CPP: auto s = getLine();
    CPP: print(string("Here is what you typed:"));
    CPP: print(s);
    CPP: return
*)
Add LoadPath "../../classes".
Require MIO.
Import MIO.IO.

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

Definition getLine : io string :=
  s <- read_loop 0;
  ret s.

Definition input_output (greeting: string) : io unit :=
  _ <- print greeting;
  s <- getLine;
  _ <- print "Here is what you typed:";
  _ <- print s;
  ret tt.

Definition main :=
  input_output "Type something and press return".

Require Extraction.
Extraction Language JSON.
Separate Extraction main.

