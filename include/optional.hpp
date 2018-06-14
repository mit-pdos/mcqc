#ifndef OPTIONAL_H
#define OPTIONAL_H
#include <optional>
#include "type_checks.hpp"

namespace optional {

    template<typename T>
    using Optional = std::optional<T>;

    template<typename T, typename Func, typename Func2, typename Ret = std::invoke_result_t<Func>>
    constexpr const auto match(Optional<T>& o, Func f, Func2 g) {
        static_assert(CallableWith<Func>, "1st argument not callable with void");
        static_assert(CallableWith<Func2, T>, "2nd argument not callable with T");
        static_assert(std::is_same<std::invoke_result_t<Func>, std::invoke_result_t<Func2, T>>::value, "Arg function return types must match");

		switch(o.has_value()) {
        case false: return f();
        case true:  return g(o.value());
        }
        // Silence warnings
        return f();
    }

    // None
    template<typename T>
    constexpr inline static Optional<T> none();
    // Constructive cons, copies t so t can be referenced again
    template<typename T>
    constexpr inline static Optional<T> some(T t);
    // Destructive cons, reuses t so it can not be referenced again
    template<typename T>
    constexpr inline static Optional<T> dsome(T t);
}
#endif
