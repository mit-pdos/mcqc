#ifndef PAIR_H
#define PAIR_H
#include <utility>

namespace Pair {
    template<class A, class B>
    using pair = std::pair<A, B>;

    template<class A, class B>
    inline static std::pair<A, B> mkpair(A&& a, B&& b) {
        return std::make_pair(std::forward<A>(a), std::forward<B>(b));
    }

    // match
    template<typename Func,
             class A, class B,
             typename Ret = std::invoke_result_t<Func, A, B>>
    constexpr Ret match(pair<A, B>&& tp, Func f) {
        return f(tp.first, tp.second);
    }
    template<typename Func,
             class A, class B,
             typename Ret = std::invoke_result_t<Func, A, B>>
    constexpr Ret match(pair<A, B>& tp, Func f) {
        return match(std::forward<pair<A,B>>(tp), f);
    }

    // First of pair
    template <class A, class B>
    constexpr auto fst(pair<A, B>&& tp) {
        return tp.first;
    }
    template <class A, class B>
    constexpr auto fst(pair<A, B>& tp) {
        return fst(std::forward<pair<A, B>>(tp));
    }

    // Second of pair
    template <class A, class B>
    constexpr auto snd(pair<A, B>&& tp) {
        return tp.second;
    }
    template <class A, class B>
    constexpr auto snd(pair<A, B>& tp) {
        return snd(std::forward<pair<A, B>>(tp));
    }
}
#endif
