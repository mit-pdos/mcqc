#include <iostream>
#include "benchmark.h"
#include "Rev.cpp"

#define BMAX 1000

using namespace std;

// Generate show methods by Generics maybe
template<typename T>
void print(std::shared_ptr<list<T>> l) {
    match(l,
        []() { std::cout << "null" << std::endl; },
        [](T t, std::shared_ptr<list<T>> tail){ std::cout << t << " "; print(tail); });
}

int main() {
	// Lists benchmark
	std::shared_ptr<list<unsigned int>> bar = coq_nil<unsigned int>();
	for(unsigned int i = 0; i < BMAX; ++i)
		bar = coq_cons(i, bar);
	// benchmark, start
	print(bar);
    tic();
	print(rev(bar));
	// benchmark, stop
	toc();
}

