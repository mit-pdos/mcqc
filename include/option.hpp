#ifndef OPTIONAL_H
#define OPTIONAL_H
#include <iostream>
#include <optional>
#include <variant>
#include "type_checks.h"
#include <iostream>

namespace Option {

    template<typename T>
    using option = std::optional<T>;

    // Constructive match, does not modify o
    // Arguments:
    //   option<T> o    : an option to pattern match on
    //   Func2 f(T val) : Lambda to call if o is not empty, takes one arguments: (T val)
    //   Func  g()      : Lambda to call if o is empty
    template<typename T,
             typename Func, typename Func2,
             typename Ret = std::invoke_result_t<Func2>,
             typename = std::enable_if_t<CallableWith<Func, T&&>
                    && "1st argument not callable with void">,
             typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func>>
                    && "Arg function return types must match">>
    constexpr Ret&& match(option<T>&& o, Func f, Func2 g) noexcept {
        if(o.has_value()) {
            return FWD(f(o.value()));
        }
        return FWD(g());
    }

    // None
    template<typename T>
    constexpr inline static option<T> none() {
        return option<T>{};
    }

    // Constructive cons, copies t so t can be referenced again
    template<typename T>
    constexpr inline static option<T> some(T t) {
        return option<T>(t);
    }
}
#endif
