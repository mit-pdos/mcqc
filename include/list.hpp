#ifndef LIST_H
#define LIST_H
#include <tuple>
#include <list>
#include "nat.hpp"
#include "optional.hpp"
#include "type_checks.h"

namespace list {
    template<typename T>
    using List = std::list<T>;

    // Constructive match, l is considered immutable and will be copied safely
    template<typename T, typename Func, typename Func2, typename Ret = std::invoke_result_t<Func>>
    inline static const Ret match(List<T> l, Func f, Func2 g) {
        static_assert(CallableWith<Func>, "1st argument not callable with void");
        static_assert(CallableWith<Func2, T, List<T>>, "2nd argument not callable with (T, List<T>)");
        static_assert(std::is_same<std::invoke_result_t<Func>, std::invoke_result_t<Func2, T, List<T>>>::value, "Arg function return types must match");

        switch(l.empty()) {
        case true:  return f();
        case false: {
            auto head = l.begin();
            return g(*head, List<T>(++head, l.end()));
        }
        }
		// Should never happen
		return f();
    }

    // Constructive cons, copies l so l can be referenced again
    template<typename T>
    inline static List<T> cons(T t, List<T> l);

    // Utility functions
    // List
    template<typename T>
    inline static optional::Optional<T> head(List<T> l);

    // Constructive tail, l is considered immutable and will be copied safely
    template<typename T>
    inline static List<T> tail(List<T> l);

    // Fully constructive app, both l1, l2 are immutable and will be copied.
    template<typename T>
    inline static List<T> app(List<T> l1, List<T> l2);

	// Higher order functions
    // Constructive mpp,
    template<typename T, typename Func>
    inline static List<T> map(Func f, List<T> l);

    // Fold right
    template<typename T1, typename T2, typename Func>
    inline static T2 foldr(Func f, T2 elem, List<T1> l);

    // Fold left
    template<typename T1, typename T2, typename Func>
    inline static T2 foldl(Func f, T2 elem, List<T1> l);

    // Constructive filter,
    template<typename T, typename Func>
    inline static List<T> filter(Func f, List<T> l);

    // Boolean
    template<typename T>
    inline static bool empty(List<T> l);
    template<typename T>
    inline static bool in(List<T> l);

    /// Arithmetic
    template<typename T>
    inline static nat::Nat length(List<T> l);
}
#endif
