#ifndef LIST_H
#define LIST_H
#include <list>
#include "nat.hpp"
#include "bool.hpp"
#include "option.hpp"
#include "type_checks.h"

using namespace Bool;
using namespace Option;
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
        if(l.empty()) {
            return f();
        }
        auto head = l.begin();
        l.pop_front();
        return g(FWD(*head), FWD(l));
    }

    // Destructive cons, modifies l and appends an element
    template<typename L,
             typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    static list<T>&& cons(T&& t, L&& l) noexcept {
        l.push_front(FWD(t));
        return FWD(l);
    }

    // Get first element of list
    template<typename L,
             typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    static option<T> head(L&& l) noexcept {
        if (FWD(l.empty())) {
            return none<T>();
        }
        return some<T>(FWD(l.front()));
    }

    // Destructive tail, l is considered mutable
    template<typename L,
             typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    static list<T>&& tail(L&& l) noexcept {
        if (FWD(l.empty())) {
            return FWD(l);
        }
        l.pop_front();
        return FWD(l);
    }

    // Fully destructive app, both l1, l2 are mutable
    template<typename L, typename R,
             typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>,
             typename = std::enable_if_t<is_same_kind_v<R, list<T>>>>
    static list<T>&& app(L&& l, R&& r) noexcept {
        // If constant, create a unique ptr
        if constexpr (is_constr_v<L> && is_constr_v<R>) {
            auto cp = std::make_unique<L>(l);
            cp->insert(l.end(), r.begin(), r.end());
            return *cp;
        } else if constexpr (is_constr_v<L>) {
            r.insert(r.begin(), l.begin(), l.end());
            return FWD(r);
        } else {
            l.splice(l.end(), r);
            return FWD(l);
        }
    }

    // Boolean
    template<typename L,
             typename = std::enable_if_t<is_same_kind_v<L, list>>>
    static bool empty(L&& l) noexcept {
        return l.empty();
    }

    /// Arithmetic
    template<typename L,
             typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    static nat length(L&& l) noexcept {
        return static_cast<nat>(l.size());
    }
}
#endif

