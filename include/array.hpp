#include <climits>
#include <type_traits>
#include "exception.h"
#include <vector>

/*
  Class NativeArray A List :=
  {
    get:    nat -> List -> A;
    put:    nat -> A -> List -> List;
    empty:  nat -> A -> List;
  }.
*/
using nat = unsigned int;

namespace Array {

    // Array definition
    template<typename T>
    using array = std::vector<T>;

    template<typename T>
    constexpr T& get(nat n, array<T>& a) {
        return a[n];
    }
    template<typename T>
    constexpr T&& get(nat n, array<T>&& a) {
        return a[n];
    }

    template<typename T>
    constexpr array<T>&& put(nat n, T val, array<T>&& a) {
        a[n] = val;
        return a;
    }

    template<typename T>
    constexpr array<T>& put(nat n, T val, array<T>& a) {
        a[n] = val;
        return a;
    }

    template<typename T>
    constexpr array<T> empty(nat n, T val) {
        return std::vector<T>(n, val);
    }
}
