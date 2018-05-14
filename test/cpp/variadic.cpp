#include <iostream>
#include <variant>
#include <string>
#include <cstdio>
#include "terms.hpp"
#include "proc.hpp"

using namespace func;

int main() {
	auto c = Term<int>(42);
	auto fun = Term<int, int, int>([](int a, int b) { return a + b; });

	Proc p;

	std::cout << c() << std::endl;
	std::cout << fun(3,4) << std::endl;
	p.print(666);

	return 0;
}
