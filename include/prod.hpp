#ifndef PROD_H
#define PROD_H
#include <utility>

namespace Prod {
    template<class A, class B>
    using prod = std::pair<A, B>;

    // match
    template<typename Func,
             class A, class B,
             typename Ret = std::invoke_result_t<Func, A, B>>
    constexpr Ret match(prod<A, B>&& tp, Func f) {
        return f(tp.first, tp.second);
    }
    template<typename Func,
             class A, class B,
             typename Ret = std::invoke_result_t<Func, A, B>>
    constexpr Ret match(prod<A, B>& tp, Func f) {
        return match(std::forward<prod<A,B>>(tp), f);
    }

    // First of pair
    template <class A, class B>
    constexpr auto fst(prod<A, B>&& tp) {
        return tp.first;
    }
    template <class A, class B>
    constexpr auto fst(prod<A, B>& tp) {
        return fst(std::forward<prod<A, B>>(tp));
    }

    // Second of pair
    template <class A, class B>
    constexpr auto snd(prod<A, B>&& tp) {
        return tp.second;
    }
    template <class A, class B>
    constexpr auto snd(prod<A, B>& tp) {
        return snd(std::forward<prod<A, B>>(tp));
    }
}
#endif
