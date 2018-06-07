#ifndef LIST_H
#define LIST_H
#include <tuple>
#include <list>
#include "nat.hpp"

namespace list {
	// Nat definition
	enum ListAtom { Nil, Cons };

    template<typename T>
	using List = std::list<T>;

    template<typename T>
	using ListTuple = const std::tuple<ListAtom, T, List<T>>; // Succ, head, tail

	// Destructive match, l is considered mutable and should not be referenced again as l
	template<typename T>
	constexpr const ListTuple<T> dmatch(List<T> l){
		switch(l.empty()) {
		case true:  return {Nil, T(), List<T>()};
		case false: return {Cons, l.front(), l.pop_front()};
		}
	}
	// Constructive match, l is considered immutable and will be copied safely
	template<typename T>
	constexpr const ListTuple<T> match(List<T> l) {
		switch(l.empty()) {
		case true:  return {Nil, List<T>()};
		case false: {
            auto head = l.begin();
			return {Cons, *head, List<T>(head++, l.end())};
		}
		}
	}

    // Constructive cons, copies l so l can be referenced again
	template<typename T>
	constexpr inline static List<T> cons(List<T>& l);
    // Destructive cons, reuses l so it can not be referenced again
	template<typename T>
	constexpr inline static List<T> dcons(List<T>& l);

	// Utility functions
    // List
    template<typename T>
    constexpr inline static T head(List<T> l);

    // Constructive tail, l is considered immutable and will be copied safely
    template<typename T>
    constexpr inline static List<T> tail(List<T> l);
    // Destructive tail, l is considered mutable and should not be referenced again as l
    template<typename T>
    constexpr inline static List<T> dtail(List<T> l);

    // Destructive app, both l1, l2 are mutable, l1 will be expanded, l2 will be deallocated. Should not be referenced again.
    template<typename T>
    constexpr inline static List<T> dappd(List<T> l1, List<T> l2);
    // Left constructive app, both l1 mutable, l2 is immutable. l1 should not be referenced again.
    template<typename T>
    constexpr inline static List<T> dapp(List<T> l1, List<T> l2);
    // Right constructive app, both l1 is immutable, l2 is mutable. l2 should not be referenced again.
    template<typename T>
    constexpr inline static List<T> appd(List<T> l1, List<T> l2);
    // Fully constructive app, both l1, l2 are immutable and will be copied.
    template<typename T>
    constexpr inline static List<T> app(List<T> l1, List<T> l2);

    // Boolean
	template<typename T>
    constexpr inline static bool empty(List<T> l);
    template<typename T>
    constexpr inline static bool in(List<T> l);

    /// Arithmetic
	template<typename T>
	constexpr inline static nat::Nat length(List<T> l);
}
#endif
