#ifndef BOOL_H
#define BOOL_H
#include <type_traits>
#include "type_checks.h"

namespace boolean {
    // Nat definition
    using Bool = bool;

    // Pattern matching bool
    template<typename Func, typename Func2, typename Ret = std::invoke_result_t<Func>,
             typename = std::enable_if_t<CallableWith<Func>   && "1st argument not callable with void">,
             typename = std::enable_if_t<CallableWith<Func2>  && "2nd argument not callable with void">,
             typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func2>> && "Arg function return types must match">>
    constexpr Ret match(Bool a, Func f, Func2 g){
        if(a) { return f(); }
		return g();
    }
}
#endif
