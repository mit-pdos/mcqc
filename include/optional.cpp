#ifndef OPTIONAL_CPP
#define OPTIONAL_CPP
#include <iostream>
#include <optional>
#include <tuple>
#include "optional.hpp"

namespace optional {
    // None
    template<typename T>
    constexpr inline static Optional<T> none() {
        return {};
    }

    // Constructive cons, copies t so t can be referenced again
    template<typename T>
    constexpr inline static Optional<T> some(T t) {
        return Optional<T>(t);
    }
}
#endif
