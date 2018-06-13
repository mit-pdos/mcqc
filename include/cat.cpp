#include <iostream>
#include <experimental/filesystem>
#include "monad.hpp"
#include "string.cpp"
#include "proc.hpp"

using Fd = proc::Fd;

/*
(** Coq definition of cat *)
Definition cat (path: pathname) (fn : filename) :=
  f <- open (path ++ [fn]);
  contents <- read f;
  _ <- close f;
  _ <- print contents;
  ret unit.
*/


// Cat a file under a dir
void cat(string::String path, string::String fn) {
    // Nest four things with continuations
    compose([&]() { return proc::open( string::dapp(path, fn) ); }, [&](Fd f) {
        compose([&]() { return proc::read(f); }, [&](string::String s) {
            compose_void([&]() { proc::close(f); }, [&]() {
				compose_void([&]() { proc::print(s); }, [&]() {
                	return;
            	});
        	});
    	});
	});
}

// --------------- Main ---------------
int main(int argc, char** argv) {
	// Get current path
	string::String path = std::experimental::filesystem::current_path();
	// Append slash as std::experimental::filesystem doesn't
	path = string::dapp(path, "/");

    // Execute Cat with coq semantics natively
	for(int i = 1; i < argc; ++i)
		cat(path, argv[i]);
}

