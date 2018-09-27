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
	list<int> bar;
	for(int i = 0; i < BMAX; ++i)
		bar.push_back(i);
	// benchmark, start
    tic();
	print(show(rev(bar)));
	// benchmark, stop
	toc();
}

