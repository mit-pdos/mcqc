#include <type_traits>
#include <variant>
#include <functional>
#include <iostream>
#include "terms.hpp"

static_assert(func::is_unit<func::unit> :: value, "std::monostate should be unit");
static_assert(!func::is_unit<int> :: value, "int should not be unit");

using namespace func;

int main() {
    auto c = Term<int>(42);
    auto fun = Term<int, int, int>([](int a, int b) { return a + b; });

    std::cout << c() << std::endl;
    std::cout << fun(3,4) << std::endl;

    return 0;
}


