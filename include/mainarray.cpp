#include "array.hpp"
#include "show.hpp"
#include "io.hpp"

#include <iostream>

using namespace Show;
using namespace IO;
using namespace Array;

int main() {
    auto a = empty(3, 0);
    auto b = put(0,5, a);
    std::cout << get(0, b) << std::endl;
}
