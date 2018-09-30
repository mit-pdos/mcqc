#ifndef LIST_H
#define LIST_H
#include <list>
#include "nat.hpp"
#include "bool.hpp"
#include "optional.hpp"
#include "type_checks.h"

using namespace Bool;
using namespace Optional;
using namespace Nat;

namespace List {

    template<typename T>
    using list = std::list<T>;

    // Destructive match, modifies l
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename Func, typename Func2,
             typename Ret = std::invoke_result_t<Func>,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>  && "Only match on list<T> types">,
             typename = std::enable_if_t<CallableWith<Func>          && "1st argument not callable with void">,
             typename = std::enable_if_t<CallableWith<Func2, T, L>   && "2nd argument not callable with (T, list<T>)">,
             typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func2, T, L>> && "Arg function return types must match">>
    static Ret match(L&& l, Func f, Func2 g) {
        switch(l.empty()) {
        case true:  return f();
        default: {
            auto head = l.begin();
            l.pop_front();
            return g(*head, FWD(l));
        }
        }
    }

    // Destructive cons, modifies l and appends an element
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    inline static list<T>&& cons(T&& t, L&& l) noexcept {
        l.push_front(FWD(t));
        return FWD(l);
    }

    // Get first element of list
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    static optional<T> head(L&& l) noexcept {
        if (FWD(l.empty())) {
            return none<T>();
        }
        return some<T>(FWD(l.front()));
    }

    // Destructive tail, l is considered mutable
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    static list<T>&& tail(L&& l) noexcept {
        if (FWD(l.empty())) {
            return FWD(l);
        }
        l.pop_front();
        return FWD(l);
    }

    // Fully destructive app, both l1, l2 are mutable
    template<typename L1, typename L2,
             typename T = typename std::remove_reference_t<L1>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L1, list<T>>>,
             typename = std::enable_if_t<is_same_kind_v<L2, list<T>>>>
    static list<T>&& app(L1&& l1, L2&& l2) noexcept {
        l1.splice(l1.end(), l2);
        return FWD(l1);
    }

    // Boolean
    template<typename L,
             typename = std::enable_if_t<is_same_kind_v<L, list>>>
    static bool empty(L&& l) noexcept {
        return l.empty();
    }

    /// Arithmetic
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    static nat length(L&& l) noexcept {
        return static_cast<nat>(l.size());
    }
}
#endif

