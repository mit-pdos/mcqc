#ifndef BOOL_H
#define BOOL_H
#include <type_traits>
#include "type_checks.h"

namespace Bool {
    // Pattern matching bool
    template<typename Func, typename Func2, typename Ret = std::invoke_result_t<Func>,
             typename = std::enable_if_t<CallableWith<Func>   && "1st argument not callable with void">,
             typename = std::enable_if_t<CallableWith<Func2>  && "2nd argument not callable with void">,
             typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func2>> && "Arg function return types must match">>
    constexpr Ret match(bool a, Func f, Func2 g){
        if(a) { return f(); }
        return g();
    }

    static inline bool negb(bool a) {
        return !a;
    }
    static inline bool andb(bool a, bool b) {
        return a & b;
    }
    static inline bool orb(bool a, bool b) {
        return a | b;
    }
    static inline bool xorb(bool a, bool b) {
        return a ^ b;
    }

}
#endif
