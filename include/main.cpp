#include <iostream>
#include <ctime>
#include <cassert>
#include "option.hpp"
#include "show.hpp"
#include "copy.hpp"
#include "list.hpp"
#include "bool.hpp"
#include "proc.hpp"
#include "tuple.hpp"
#include "string.hpp"
#include "curry.h"
#include "benchmark.h"
#include "type_checks.h"

#define BMAX 1000

using namespace Option;
using namespace Nat;
using namespace List;
using namespace String;
using namespace Tuple;
using namespace Bool;
using namespace Show;
using namespace Copy;
using namespace Proc;

template<typename T, typename Func>
list<T> mapl(Func f, list<T> l) {
    return match(l,
        [=]()    { return list<T>(); },
        [=](auto h, auto ls) { return cons(f(h), mapl(f, ls)); });
}

template<typename T>
static inline list<T> rev(list<T> l) {
    return match(l,
        []()    { return list<T>(); },
        [](auto h, auto ls) { return app(rev(ls), list<T>(1, h)); });
}

int main() {

    // Currying
    auto f = [](auto a, auto b, auto c, auto d) {
        return a  * b * c * d;
    };

    list<nat> lval = list<nat>{2,4,6,8};
    list<nat> rval = list<nat>{1,3,5};

    list<nat> zeros = list<nat>{0,0,0};
    list<list<nat>> llval = list<list<nat>>{lval, rval, lval, rval};
    list<list<nat>> zzval = list<list<nat>>{zeros, zeros};

    print(mkstring('c',string("elo")));
    print(show(app(app(copy(llval), zzval), copy(llval))));
    print(show(head(llval)));
    // Move some lists around to test rval references
    print(show(app(copy(lval), list<nat>{1,3,5})));
    print(show(app(list<nat>{2,4,6,8}, list<nat>{1,3,5})));
    print(show(app(copy(lval), rval)));
    print(show(app(list<nat>{2,4,6,8}, rval)));
    print(show(tail(list<nat>{2,4,6,8})));
    print(show(tail(copy(lval))));
    print(show(tail(copy(lval))));
    print(show(tail(copy(lval))));

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

    // Lists with initializer_list
    print(show(rev(list<int>{1,2,3})));

    // High order logic (map)
    print(show(mapl([](int n) { return n * 2; }, list<int>{1,2,3})));

    // Lists benchmark
    list<int> bar;
    for(int i = 0; i < BMAX; ++i)
        bar.push_back(i);
    tic();
    assert(rev(rev(bar)) == bar);
    toc();

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

    // Tuples
    auto t = mktuple(1, 'b', "foo");
    match(t,
        [](int a, char b, const char* c) {
            std::cout << "Tuple expanded: " << a << ", " << b << ", " << c << std::endl;
        });
    std::cout << "snd(t): " << snd(t) << std::endl;
    std::cout << "fst(t): "; print(show(fst(t)));
    std::cout << "fst(fst(t)): "; print(show(fst(fst(t))));
    return 0;
}

