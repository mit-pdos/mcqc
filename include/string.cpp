#include <string>
#include "string.hpp"
#include "nat.hpp"

namespace string {

    // Constructive cons, copies l so l can be referenced again
    inline static String cons(String& l, Char h) {
        String s = String(l);
        s.insert(s.begin(), h);
        return s;
    }
    // Destructive cons, reuses l so it can not be referenced again
    inline static String dcons(String& l, Char h) {
        l.insert(l.begin(), h);
        return l;
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
    // Destructive tail, l is considered mutable and should not be referenced again as l
    inline static String dtail(String l) {
        l.erase(l.begin());
        return l;
    }

    // Left constructive app, both l1 mutable, l2 is immutable. l1 should not be referenced again.
    inline static String dapp(String l1, String l2) {
        l1.append(l2);
        return l1;
    }
    // Right constructive app, both l1 is immutable, l2 is mutable. l2 should not be referenced again.
    inline static String appd(String l1, String l2) {
        l2.insert(0, l1);
        return l2;
    }
    // Fully constructive app, both l1, l2 are immutable and will be copied.
    inline static String app(String l1, String l2) {
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
