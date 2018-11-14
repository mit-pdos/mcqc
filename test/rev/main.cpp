#include "show.hpp"
#include "proc.hpp"
#include "benchmark.h"
#include "Datatypes.cpp"
#include "Rev.cpp"

#define BMAX 1000

using namespace Proc;
using namespace Show;

int main() {
	// Lists benchmark
	std::shared_ptr<list<unsigned int>> bar = coq_nil<unsigned int>();
	for(unsigned int i = 0; i < BMAX; ++i)
		bar = coq_cons(i, bar);
	// benchmark, start
	print(show(bar));
    tic();
	print(show(rev(bar)));
	// benchmark, stop
	toc();
}

