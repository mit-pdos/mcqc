#include <iostream>
#include <ctime>
#include <cassert>
#include "option.hpp"
#include "show.hpp"
#include "bool.hpp"
#include "proc.hpp"
#include "pair.hpp"
#include "string.hpp"
#include "curry.h"
#include "benchmark.h"
#include "type_checks.h"

#define BMAX 1000

using namespace Option;
using namespace Nat;
using namespace Pair;
using namespace String;
using namespace Bool;
using namespace Show;
using namespace Proc;


int main() {

    print(show(true));

    // Create a UUID v4
    print(getuuid());

    print(show(mkpair((nat)1, string("foo"))));

    // Currying
    auto f = [](auto a, auto b, auto c, auto d) {
        return a  * b * c * d;
    };

    std::cout << "Currying 4! = " << curry(f)(1)(2)(3)(4) << std::endl;
    // Optional Switch
    match(some(42),
        [](int m) { std::cout << "Some " << m << std::endl; },
        []()      { std::cout << "Empty" << std::endl; });

    // Optional print
    print(show(some<nat>(41)));
    print(show(none<nat>()));
    // Async
    spawn([](string s) { print(s); }, string("Not necessarily"));
    spawn([](nat n) { print(show(n)); }, random());
    spawn([](string s) { print(s); }, string("In order"));

    // Bools
    match((bool)false,
        []() { throw IOException("Should never be matched, since bool is false"); },
        []() { std::cout << "Bool matching works" << std::endl; });

    // strings
    match(append(string("foo"), string("bar")),
        []() { throw IOException("Should never be matched, since the str is non-empty"); },
        [](auto h, auto ts) {
            assert(h == 'f' && ts == "oobar");
            std::cout << "string matching works" << std::endl;
        });

    string foo = "foo";
    string baz = "baz";
    print(append(string(foo),baz));
    print(append(string(foo),baz));
    print(foo);

    // Pairs
    auto t = mkpair((nat)1, 'b');
    match(t,
        [](nat a, char b) {
            std::cout << "Pair expanded: " << a << ", " << b << std::endl;
        });

    std::cout << "fst(t): " << fst(t) << std::endl;
    std::cout << "snd(t): " << snd(t) << std::endl;
    return 0;
}

