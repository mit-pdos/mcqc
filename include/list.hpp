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

    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename Func, typename Func2,
             typename Ret = std::invoke_result_t<Func>,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>  && "Only match on list<T> types">,
             typename = std::enable_if_t<CallableWith<Func>          && "1st argument not callable with void">,
             typename = std::enable_if_t<CallableWith<Func2, T, L>   && "2nd argument not callable with (T, list<T>)">,
             typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func2, T, L>> && "Arg function return types must match">>
    static Ret match(L&& l, Func f, Func2 g) {
        switch(FWD(l.empty())) {
        case true:  return f();
        default: {
            auto head = FWD(l.begin());
            return g(*head, list<T>(++head, FWD(l.end())));
        }
        }
    }

    // TODO: Make not copy
    // Constructive cons, copies l so l can be referenced again
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    inline static list<T> cons(T t, L&& l) {
        auto l2 = list<T>(l);
        l2.push_front(t);
        return l2;
    }

    // Utility functions
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    static optional<T> head(L&& l) {
        if (FWD(l.empty())) {
            return none<T>();
        }
        return some<T>(FWD(l.front()));
    }

    // Constructive tail, l is considered immutable and will be copied safely
 	template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    static list<T> tail(L&& l) {
        auto head = FWD(l.begin());
        return list<T>(++head, FWD(l.end()));
    }

    // Fully constructive app, both l1, l2 are immutable and will be copied.
    template<typename L1, typename L2,
             typename T = typename std::remove_reference_t<L1>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L1, list<T>>>,
             typename = std::enable_if_t<is_same_kind_v<L2, list<T>>>>
    inline static list<T> app(L1&& l1, L2&& l2) {
        list<T> l3 = list<T>(l1);
        l3.insert(l3.end(), l2.begin(), l2.end());
        return l3;
    }

    // Boolean
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    static bool empty(L&& l) {
        return FWD(l.empty());
    }

    /// Arithmetic
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, list<T>>>>
    static nat length(L&& l) {
        return static_cast<nat>(FWD(l.size()));
    }
}
#endif

