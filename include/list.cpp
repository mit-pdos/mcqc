#include <list>
#include "nat.hpp"
#include "list.hpp"

namespace list {

    // Constructive cons, copies l so l can be referenced again
	template<typename T>
	inline static List<T> cons(List<T>& l, T t) {
		auto l2 = List<T>(l);
		l2.push_front(t);
		return l2;
    }
    // Destructive cons, reuses l so it can not be referenced again
	template<typename T>
	inline static List<T> dcons(List<T>& l, T t) {
		l.push_front(t);
		return l;
    }

	// Utility functions
    // List
    template<typename T>
    inline static T head(List<T> l) {
		return l.front();
    }

    // Constructive tail, l is considered immutable and will be copied safely
    template<typename T>
    inline static List<T> tail(List<T> l) {
		auto head = l.begin();
        return List<T>(head++, l.end());
	}
    // Destructive tail, l is considered mutable and should not be referenced again as l
    template<typename T>
    inline static List<T> dtail(List<T> l) {
		l.pop_front();
		return l;
    }

    // Destructive app, both l1, l2 are mutable, l1 will be expanded, l2 will be deallocated. Neither should be referenced again.
    template<typename T>
    inline static List<T> dappd(List<T> l1, List<T> l2) {
		l1.splice(l1.end(), l2);
		delete l2;
		return l1;
	}
    // Left constructive app, both l1 mutable, l2 is immutable. l1 should not be referenced again.
    template<typename T>
    inline static List<T> dapp(List<T> l1, List<T> l2) {
		l1.splice(l1.end(), l2);
		return l1;
	}
    // Right constructive app, both l1 is immutable, l2 is mutable. l2 should not be referenced again.
    template<typename T>
    inline static List<T> appd(List<T> l1, List<T> l2) {
		l2.splice(l2.begin(), l1);
		return l2;
	}
    // Fully constructive app, both l1, l2 are immutable and will be copied.
    template<typename T>
    inline static List<T> app(List<T> l1, List<T> l2) {
		List<T> l3 = List<T>(l1);
		l3.splice(l3.end(), l2);
		return l3;
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
