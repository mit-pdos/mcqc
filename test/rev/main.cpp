#include "list.hpp"
#include "show.hpp"
#include "proc.hpp"
#include "benchmark.h"
#include "Rev.cpp"

#define BMAX 1000

using namespace List;
using namespace Proc;
using namespace Show;

int main() {
	// Lists benchmark
	list<unsigned int> bar;
	for(unsigned int i = 0; i < BMAX; ++i)
		bar.push_back(i);
	// benchmark, start
	print(show(bar));
    tic();
	print(show(rev(bar)));
	// benchmark, stop
	toc();
}

