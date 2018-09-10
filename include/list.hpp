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
            return optional::none();
        }
        return optional::some(FWD(l.front()));
    }

    // Constructive tail, l is considered immutable and will be copied safely
 	template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>>
    static List<T> tail(L&& l) {
        auto head = FWD(l).begin();
        return List<T>(++head, FWD(l).end());
    }

    // Fully constructive app, both l1, l2 are immutable and will be copied.
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>>
    inline static List<T> app(L&& l1, L&& l2) {
        List<T> l3 = List<T>(l1);
        l3.splice(l3.end(), l2);
        return l3;
    }

    // Higher order functions
    // Constructive mpp,
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename Func,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>,
             typename = std::enable_if_t<CallableWith<Func, T> && "Function not callable with type T">,
		     typename = std::enable_if_t<std::is_same_v<std::invoke_result_t<Func, T>, T> && "Function return type must match T">>
    static List<T> map(Func f, L&& l) {
        // Create new list
        List<T> l2 = List<T>(l);
        for (auto it = l2.begin(); it != l2.end(); ++it) {
            *it = f(it);
        }
        return l2;
    }

    // Fold right
    template<typename L, typename T2, typename T1 = typename std::remove_reference_t<L>::value_type,
             typename Func,
             typename = std::enable_if_t<is_same_kind_v<L, List<T1>>>,
     		 typename = std::enable_if_t<CallableWith<Func, T1, T2> && "Function not callable with type T1, T2">,
             typename = std::enable_if_t<std::is_same_v<std::invoke_result_t<Func, T1, T2>, T2> && "Function must be T1 -> T2 -> T2">>
    static T2 foldr(Func f, T2&& elem, L&& l) {
        // Empty list, return element
        if (FWD(l.empty())) {
            return elem;
        }
        T2 buffer = f(FWD(*l.end()), FWD(elem));
        for (auto it = FWD(l.end()); it != FWD(l.begin()); --it) {
            auto prev = it - 1;
            buffer = f(*prev, buffer);
        }
        return buffer;
    }

    // Fold left
    template<typename L, typename T2, typename T1 = typename std::remove_reference_t<L>::value_type,
             typename Func,
             typename = std::enable_if_t<is_same_kind_v<L, List<T1>>>,
             typename = std::enable_if_t<CallableWith<Func, T1, T2> && "Function not callable with type T1, T2">,
             typename = std::enable_if_t<std::is_same_v<std::invoke_result_t<Func, T1, T2>, T2> && "Function must be T1 -> T2 -> T2">>
    static T2 foldl(Func f, T2 elem, L&& l) {
        // Empty list, return element
        if (l.empty()) {
            return elem;
        }
        // Empty list, apply once
        T2 buffer = f(*(l.begin()), elem);
        for (auto it = l.begin() + 1; it != l.end(); ++it) {
            buffer = f(*it, buffer);
        }
        return buffer;
    }

    // Constructive filter,
 	template<typename L, typename Func,
			 typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>,
			 typename = std::enable_if_t<CallableWith<Func, T> && "Function not callable with type T">,
             typename = std::enable_if_t<std::is_same_v<std::invoke_result_t<Func, T>, Bool> && "Function must be T -> Bool">>
    static List<T> filter(Func f, L&& l) {
        List<T> l2 = List<T>(l);
        return filterd(l2, f);
    }

    // Boolean
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>>
    static Bool empty(L&& l) {
        return FWD(l.empty());
    }

    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>>
    inline static Bool in(L&& l, T t) {
        for (auto it = FWD(l.begin()); it != FWD(l.end()); it++) {
            if(*it == t)
                return true;
        }
        return false;
    }
    /// Arithmetic
    template<typename L, typename T = typename std::remove_reference_t<L>::value_type,
             typename = std::enable_if_t<is_same_kind_v<L, List<T>>>>
    static nat::Nat length(L&& l) {
        return static_cast<nat::Nat>(FWD(l.size()));
    }
}
#endif

