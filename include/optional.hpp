#ifndef OPTIONAL_H
#define OPTIONAL_H
#include <iostream>
#include <optional>
#include <variant>
#include "type_checks.h"

namespace optional {

    template<typename T>
    using Optional = std::optional<T>;

    template<typename O, typename T = typename std::remove_reference_t<O>::value_type,
             typename Func, typename Func2,
             typename Ret = std::invoke_result_t<Func2>,
             typename = std::enable_if_t<is_same_kind_v<O, Optional<T>>>>
    constexpr Ret match(O&& o, Func f, Func2 g) {
        static_assert(CallableWith<Func, T>, "1st argument not callable with T");
        static_assert(CallableWith<Func2>, "2nd argument not callable with void");
        static_assert(std::is_same<std::invoke_result_t<Func2>, std::invoke_result_t<Func, T>>::value, "Arg function return types must match");

        if (o.has_value()) {
            return f(o.value());
        } 
        return g();
    }

    // None
    constexpr inline static Optional<std::monostate> none() {
        return {};
    }

    // Constructive cons, copies t so t can be referenced again
    template<typename T>
    constexpr inline static Optional<T> some(T t) {
        return Optional<T>(t);
    }
}
#endif
