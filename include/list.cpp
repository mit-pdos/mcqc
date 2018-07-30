#ifndef LIST_CPP
#define LIST_CPP
#include <list>
#include "nat.hpp"
#include "list.hpp"
#include "optional.hpp"

namespace list {

    // Constructive cons, copies l so l can be referenced again
    template<typename T>
    inline static List<T> cons(T t, List<T> l) {
        auto l2 = List<T>(l);
        l2.push_front(t);
        return l2;
    }

    // Utility functions
    // List
    template<typename T>
    inline static optional::Optional<T> head(List<T> l) {
        if (l.empty()) {
            return optional::none<T>();
        }
        return optional::some(l.front());
    }

    // Constructive tail, l is considered immutable and will be copied safely
    template<typename T>
    inline static List<T> tail(List<T> l) {
        auto head = l.begin();
        return List<T>(++head, l.end());
    }

    // Fully constructive app, both l1, l2 are immutable and will be copied.
    template<typename T>
    inline static List<T> app(List<T> l1, List<T> l2) {
        List<T> l3 = List<T>(l1);
        l3.splice(l3.end(), l2);
        return l3;
    }

    // Higher order functions
    // Constructive mpp,
    template<typename T, typename Func>
    inline static List<T> map(Func f, List<T> l) {
        // Ensure Func: T -> T
        static_assert(CallableWith<Func, T>, "Function not callable with type T");
        static_assert(std::is_same<std::invoke_result_t<Func, T>, T>::value, "Function return type must match T");
        // Create new list
        List<T> l2 = List<T>(l);
		for (auto it = l2.begin(); it != l2.end(); ++it) {
            *it = f(it);
        }
        return l2;
    }

    // Fold right
    template<typename T1, typename T2, typename Func>
    inline static T2 foldr(Func f, T2 elem, List<T1> l) {
        // Ensure Func: T -> Y -> Y
        static_assert(CallableWith<Func, T1, T2>, "Function not callable with type T1, T2");
        static_assert(std::is_same<std::invoke_result_t<Func, T1, T2>, T2>::value, "Function must be T1 -> T2 -> T2");
        // Empty list, return element
        if (l.empty()) {
            return elem;
        }
        T2 buffer = f(*(l.end()), elem);
        for (auto it = l.end(); it != l.begin(); --it) {
            auto prev = it - 1;
            buffer = f(*prev, buffer);
        }
        return buffer;
    }

    // Fold left
    template<typename T1, typename T2, typename Func>
    inline static T2 foldl(Func f, T2 elem, List<T1> l) {
        // Ensure Func: T -> Y -> Y
        static_assert(CallableWith<Func, T1, T2>, "Function not callable with type T1, T2");
        static_assert(std::is_same<std::invoke_result_t<Func, T1, T2>, T2>::value, "Function must be T1 -> T2 -> T2");
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
    template<typename T, typename Func>
    inline static List<T> filter(Func f, List<T> l) {
        // Ensure Func: T -> bool
        static_assert(CallableWith<Func, T>, "Function not callable with type T");
        static_assert(std::is_same<std::invoke_result_t<Func, T>, bool>::value, "Function must be T -> bool");
        List<T> l2 = List<T>(l);
        return filterd(l2, f);
    }

    // Boolean
    template<typename T>
    inline static bool empty(List<T> l) {
        return l.empty();
    }
    template<typename T>
    inline static bool in(List<T> l, T t) {
        for (auto it = l.begin(); it != l.end(); it++) {
            if(*it == t)
                return true;
        }
        return false;
    }
    /// Arithmetic
    template<typename T>
    inline static nat::Nat length(List<T> l) {
        return static_cast<nat::Nat>(l.length());
    }
}
#endif
