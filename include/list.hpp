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
    // Arguments:
    //   list<T> l : a list to pattern match on
    //   Func  f   : Lambda to call if l is empty
    //   Func2 g   : Lambda to call if l is not empty, takes two arguments: (T head, list<T> tail)
    template<typename T,
             typename Func, typename Func2,
             typename Ret = std::invoke_result_t<Func2, T&&, list<T>&&>,
             typename = std::enable_if_t<CallableWith<Func>
                    && "1st argument not callable with void">,
             typename = std::enable_if_t<CallableWith<Func2, T&&, list<T>&&>
                    && "2nd argument not callable with (T, list<T>)">,
             typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func>>
                    && "Arg function return types must match">>
    static Ret&& match(list<T>&& l, Func f, Func2 g) noexcept {
        if(l.empty()) {
            return FWD(f());
        }
        T head = l.front();
        l.pop_front();
        return FWD(g(std::move(head), std::move(l)));
    }
    template<typename T,
             typename Func, typename Func2,
             typename Ret = std::invoke_result_t<Func2, T&&, list<T>&&>,
             typename = std::enable_if_t<CallableWith<Func>
                    && "1st argument not callable with void">,
             typename = std::enable_if_t<CallableWith<Func2, T&&, list<T>&&>
                    && "2nd argument not callable with (T, list<T>)">,
             typename = std::enable_if_t<std::is_same_v<Ret, std::invoke_result_t<Func>>
                    && "Arg function return types must match">>
    static Ret&& match(list<T>& l, Func f, Func2 g) noexcept {
        if(l.empty()) {
            return std::move(f());
        }
        T head = l.front();
        l.pop_front();
        return std::move(g(FWD(head), std::move(l)));
    }

    /*
    template<typename T, typename Func, typename Func2>
    static auto match(const list<T>& l, Func f, Func2 g) {
        // Can never be called with a const list, use copy semantics instead
        static_assert(false && "The argument l is `const` qualified and not allowed, use `copy` semantics instead");
    }
    */
    // Destructive cons, modifies l, creates prvalue of t with a unique pointer
    // for garbage collection if needed
    // Arguments:
    //   T t       : element to append to head of list
    //   list<T> l : a list that represents the tail of the returned list
    template<typename T>
    static list<T>&& cons(T&& t, list<T>& l) noexcept {
        // Create a T so list does not end up being list<T&>
        auto tptr = std::make_unique<remove_cvref_t<T>>(std::move(t));
        l.push_front(*tptr);
        return std::move(l);
    }
    template<typename T>
    static list<T>&& cons(T&& t, list<T>&& l) noexcept {
        auto tptr = std::make_unique<remove_cvref_t<T>>(std::move(t));
        l.push_front(*tptr);
        return std::move(l);
    }
    template<typename T>
    static list<T>&& cons(T& t, list<T>& l) noexcept {
        // Create a T so list does not end up being list<T&>
        auto tptr = std::make_unique<remove_cvref_t<T>>(std::move(t));
        l.push_front(*tptr);
        return std::move(l);
    }
    template<typename T>
    static list<T>&& cons(T& t, list<T>&& l) noexcept {
        auto tptr = std::make_unique<remove_cvref_t<T>>(std::move(t));
        l.push_front(*tptr);
        return std::move(l);
    }



    // Destructive head, modifies l
    // Arguments:
    //   list<T> l : a list to pop the head from
    template<typename T>
    static option<T>&& head(list<T>& l) noexcept {
        if (l.empty()) {
            return std::move(none<T>());
        }
        return std::move(some<T>(std::move(l.front())));
    }
    template<typename T>
    static option<T>&& head(list<T>&& l) noexcept {
        if (l.empty()) {
            return std::move(none<T>());
        }
        return std::move(some<T>(std::move(l.front())));
    }

    // Destructive tail, modifies l
    // Arguments:
    //   list<T> l : a list to pop the head and return the tail
    template<typename T>
    static list<T>&& tail(list<T>& l) noexcept {
        if (l.empty()) {
            return std::move(FWD(l));
        }
        l.pop_front();
        return std::move(l);
    }
    template<typename T>
    static list<T>&& tail(list<T>&& l) noexcept {
        if (l.empty()) {
            return std::move(l);
        }
        l.pop_front();
        return std::move(l);
    }

    // Destructive app, modifies both l and r
    // Arguments:
    //   list<T> l : a list to append to
    //   list<T> r : a list that becomes empty after being moved from
    template<typename T>
    static list<T>&& app(list<T>& l, list<T>& r) noexcept {
        l.splice(l.end(), std::move(r));
        return std::move(l);
    }
    template<typename T>
    static list<T>&& app(list<T>&& l, list<T>& r) noexcept {
            l.splice(l.end(), std::move(r));
            return std::move(l);
    }
    template<typename T>
    static list<T>&& app(list<T>& l, list<T>&& r) noexcept {
            l.splice(l.end(), std::move(r));
            return std::move(l);
    }
    template<typename T>
    static list<T>&& app(list<T>&& l, list<T>&& r) noexcept {
            l.splice(l.end(), std::move(r));
            return std::move(l);
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

