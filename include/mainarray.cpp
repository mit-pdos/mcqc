#include "array.hpp"
#include "show.hpp"
#include "proc.hpp"

#include <iostream>

using namespace Show;
using namespace Proc;
using namespace Array;

int main() {
    auto a = empty(3, 0);
    auto b = put(0,5, a);
    std::cout << get(0, b) << std::endl;
}
