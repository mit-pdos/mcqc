#ifndef STRING_H
#define STRING_H
#include <tuple>
#include <string>
#include <iostream>
#include "nat.hpp"

// TODO: Make everything constexpr
namespace string {
	// String definition
	enum StringAtom { Nil, Cons };

	using Char = char;
	using String = std::string;

	using StringTuple = const std::tuple<StringAtom, Char, String>; // Succ, head, tail

	// Destructive match, l is considered mutable and should not be referenced again as l
	const StringTuple dmatch(String l){
		switch(l.empty()) {
		case true:  return {Nil, 0, String()};
		case false: {
			Char head = l[0];
			l.erase(l.begin());
			return {Cons, head, l};
		}
		}
		std::cerr << "Match fell through on: " << __FILE__ << ":" << __LINE__ << std::endl;
		// Silence warning
		return {Nil, 0, String() };
	}

	// Constructive match, l is considered immutable and will be copied safely
	const StringTuple match(String l) {
		switch(l.empty()) {
		case true:  return {Nil, 0, String()};
		case false: {
            auto head = l.begin();
			return {Cons, *head, String(head++, l.end())};
		}
		}
		std::cerr << "Match fell through on: " << __FILE__ << ":" << __LINE__ << std::endl;
		// Silence warning
		return {Nil, 0, String() };
	}

    // Constructive cons, copies l so l can be referenced again
	inline static String cons(String& l, Char h);
    // Destructive cons, reuses l so it can not be referenced again
	inline static String dcons(String& l, Char h);

	// Utility functions
    // List
    inline static Char head(String l);

    // Constructive tail, l is considered immutable and will be copied safely
    inline static String tail(String l);
    // Destructive tail, l is considered mutable and should not be referenced again as l
    inline static String dtail(String l);

    // Left constructive app, both l1 mutable, l2 is immutable. l1 should not be referenced again.
    inline static String dapp(String l1, String l2);
    // Right constructive app, both l1 is immutable, l2 is mutable. l2 should not be referenced again.
    inline static String appd(String l1, String l2);
    // Fully constructive app, both l1, l2 are immutable and will be copied.
    inline static String app(String l1, String l2);

    // Boolean
    inline static bool empty(String l);
    inline static bool in(String l, Char t);

    /// Arithmetic
	inline static nat::Nat length(String l);
}
#endif
