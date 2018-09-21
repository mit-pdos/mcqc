#ifndef LIST_H
#define LIST_H
#include <list>
#include "nat.hpp"
#include "bool.hpp"
#include "optional.hpp"
#include "type_checks.h"

using namespace boolean;

namespace list {

    template<typename T>
    using List = std::list<T>;

    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename Func, typename Func2,
             typename Ret = std::invoke_result_t<Func>,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>  && "Only match on List<T> types">,
             typename = std::enable_if_t<CallableWith<Func>          && "1st argument not callable with void">,
             typename = std::enable_if_t<CallableWith<Func2, T, L>   && "2nd argument not callable with (T, List<T>)">,
             typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func2, T, L>> && "Arg function return types must match">>
    static Ret match(L&& l, Func f, Func2 g) {
        switch(FWD(l.empty())) {
        case true:  return f();
        default: {
            auto head = FWD(l.begin());
            return g(*head, List<T>(++head, FWD(l.end())));
        }
        }
    }

    // TODO: Make not copy
    // Constructive cons, copies l so l can be referenced again
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>>
    inline static List<T> cons(T t, L&& l) {
        auto l2 = List<T>(l);
        l2.push_front(t);
        return l2;
    }

    // Utility functions
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>>
    static optional::Optional<T> head(L&& l) {
        if (FWD(l.empty())) {
            return optional::none<T>();
        }
        return optional::some<T>(FWD(l.front()));
    }

    // Constructive tail, l is considered immutable and will be copied safely
 	template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>>
    static List<T> tail(L&& l) {
        auto head = FWD(l.begin());
        return List<T>(++head, FWD(l.end()));
    }

    // Fully constructive app, both l1, l2 are immutable and will be copied.
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>>
    inline static List<T> app(L&& l1, L&& l2) {
        List<T> l3 = List<T>(l1);
        l3.splice(l3.end(), FWD(l2));
        return l3;
    }

    // Boolean
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>>
    static Bool empty(L&& l) {
        return FWD(l.empty());
    }

    /// Arithmetic
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>>
    static nat::Nat length(L&& l) {
        return static_cast<nat::Nat>(FWD(l.size()));
    }
}
#endif

