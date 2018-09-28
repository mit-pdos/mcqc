#ifndef COPY_H
#define COPY_H
#include <iostream>
#include <future>
#include "string.hpp"
#include "list.hpp"
#include "optional.hpp"
#include "nat.hpp"
#include "type_checks.h"
#include "tuple.hpp"

using namespace Nat;
using namespace String;
using namespace List;
using namespace Optional;
using namespace Tuple;

namespace Copy {

    // Should be a nop
    template<typename N, typename = std::enable_if_t<is_same_kind_v<nat, N>>>
    constexpr N& copy(N&& n) noexcept {
        return FWD(n);
    }

    // copy due to the return value being a non-ref
    template<typename S=string, typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static string copy(S&& s) noexcept {
        return FWD(s);
    }

    // copy due to the return value being a non-ref
    template<typename L,
			 typename T = typename std::remove_reference_t<L>::value_type>
	static typename std::enable_if<is_same_kind_v<L, list<T>>, list<T>>::type
	copy(L&& l) {
		return FWD(l);
    }

    // copy due to the return value being a non-ref
    template<typename O,
	         typename T = typename std::remove_reference_t<O>::value_type>
    typename std::enable_if<is_same_kind_v<O, optional<T>>, optional<T>>::type
    copy(O&& o) {
        return FWD(o);
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
