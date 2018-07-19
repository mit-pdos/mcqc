#include "list.hpp"
#include "proc.hpp"
#include "benchmark.h"
#include "Rev.cpp"

#define BMAX 1000

using namespace list;

int main() {
	// Lists benchmark
	List<int> bar;
	for(int i = 0; i < BMAX; ++i)
		bar.push_back(i);
	// benchmark, start
    tic();
	proc::print(rev(bar));
	// benchmark, stop
	toc();
}

