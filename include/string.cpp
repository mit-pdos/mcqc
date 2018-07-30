#ifndef STRING_CPP
#define STRING_CPP
#include <string>
#include "optional.hpp"
#include "string.hpp"
#include "nat.hpp"

namespace string {

    // Constructive cons, copies l so l can be referenced again
    inline static String cons(String l, Char h) {
        String s = String(l);
        s.insert(s.begin(), h);
        return s;
    }
    // Utility functions
    // List
    inline static Char head(String l) {
        return l[0];
    }
    // Constructive tail, l is considered immutable and will be copied safely
    inline static String tail(String l) {
        return String(l.begin()+1, l.end());
    }

    // Fully constructive app, both l1, l2 are immutable and will be copied.
    inline static String append(String l1, String l2) {
        String s = String(l1);
        s.append(l2);
        return s;
    }
    // Boolean
    inline static bool empty(String l) {
        return l.empty();
    }
    inline static bool in(String l, Char t) {
        for (auto it = l.begin(); it != l.end(); it++) {
            if(*it == t)
                return true;
        }
        return false;
    }
    /// Arithmetic
    inline static nat::Nat length(String l) {
        return static_cast<nat::Nat>(l.length());
    }
}
#endif
