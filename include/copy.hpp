#ifndef COPY_H
#define COPY_H
#include "string.hpp"
#include "nat.hpp"
#include "type_checks.h"
#include "tuple.hpp"

using namespace Nat;
using namespace String;
using namespace Tuple;

namespace Copy {

    // Should be a nop
    template<typename N>
    typename std::enable_if_t<is_same_kind_v<nat, N>, N&>
    constexpr copy(N&& n) noexcept {
        return FWD(n);
    }

    // copy due to the return value being a non-ref
    template<typename S=string, typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static string copy(S&& s) noexcept {
        return FWD(s);
    }

    // copy due to the return value being a non-ref
    template<class TupType, size_t... I>
    static TupType copy(const TupType& t, std::index_sequence<I...>)
    {
        return FWD(t);
    }
    template<class ...Args>
    static tuple<Args...> copy(const tuple<Args...>& t)
    {
        return FWD(t);
    }
}
#endif
