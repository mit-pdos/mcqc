#include <iostream>
#include "optional.cpp"

using namespace optional;

int main() {

    Optional<int> o = Optional<int>(42);
	// Switch 1
    auto m = match(o);
    switch(std::get<0>(m)) {
    case None: {
        std::cout << "Empty optional" << std::endl;
        break;
    }
    case Some: std::cout << "Some " << std::get<1>(m) << std::endl;
    }

    auto a = none<int>();
	// Switch 2
    auto m2 = match(a);
    switch(std::get<0>(m2)) {
    case None: {
        std::cout << "None" << std::endl;
        break;
    }
    case Some: std::cout << "Some " << std::get<1>(m2) << std::endl;
    }

    return 0;
}

