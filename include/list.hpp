#ifndef LIST_H
#define LIST_H
#include <tuple>
#include <list>
#include "nat.hpp"
#include "type_checks.hpp"

namespace list {
    template<typename T>
    using List = std::list<T>;

    // Destructive match, l is considered mutable and should not be referenced again as l
    template<typename T, typename Func, typename Func2, typename Ret = std::invoke_result_t<Func>>
    inline static const Ret dmatch(List<T> l, Func f, Func2 g){
        static_assert(CallableWith<Func>, "1st argument not callable with void");
        static_assert(CallableWith<Func2, T, List<T>>, "2nd argument not callable with (T, List<T>");
        static_assert(std::is_same<std::invoke_result_t<Func>, std::invoke_result_t<Func2, T, List<T>>>::value, "Arg function return types must match");

		switch(l.empty()) {
        case true:  return f();
        case false: return g(l.front(), l.pop_front());
        }
    }

    // Constructive match, l is considered immutable and will be copied safely
    template<typename T, typename Func, typename Func2, typename Ret = std::invoke_result_t<Func>>
    inline static const Ret match(List<T> l, Func f, Func g) {
        static_assert(CallableWith<Func>, "1st argument not callable with void");
        static_assert(CallableWith<Func2, T, List<T>>, "2nd argument not callable with (T, List<T>)");
        static_assert(std::is_same<std::invoke_result_t<Func>, std::invoke_result_t<Func2, T, List<T>>>::value, "Arg function return types must match");

        switch(l.empty()) {
        case true:  return f();
        case false: {
            auto head = l.begin();
            return g(*head, List<T>(head++, l.end()));
        }
        }
    }

    // Constructive cons, copies l so l can be referenced again
    template<typename T>
    inline static List<T> cons(List<T>& l, T t);
    // Destructive cons, reuses l so it can not be referenced again
    template<typename T>
    inline static List<T> dcons(List<T>& l, T t);

    // Utility functions
    // List
    template<typename T>
    inline static T head(List<T> l);

    // Constructive tail, l is considered immutable and will be copied safely
    template<typename T>
    inline static List<T> tail(List<T> l);
    // Destructive tail, l is considered mutable and should not be referenced again as l
    template<typename T>
    inline static List<T> dtail(List<T> l);

    // Destructive app, both l1, l2 are mutable, l1 will be expanded, l2 will be deallocated. Should not be referenced again.
    template<typename T>
    inline static List<T> dappd(List<T> l1, List<T> l2);
    // Left constructive app, both l1 mutable, l2 is immutable. l1 should not be referenced again.
    template<typename T>
    inline static List<T> dapp(List<T> l1, List<T> l2);
    // Right constructive app, both l1 is immutable, l2 is mutable. l2 should not be referenced again.
    template<typename T>
    inline static List<T> appd(List<T> l1, List<T> l2);
    // Fully constructive app, both l1, l2 are immutable and will be copied.
    template<typename T>
    inline static List<T> app(List<T> l1, List<T> l2);

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
