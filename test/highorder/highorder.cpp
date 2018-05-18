#include <iostream>
#include "type_deduction.hpp"
#include <functional>
#include <variant>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

// Unit type
typedef std::monostate unit;

template<typename T>
struct Ret {
    Ret(T t) : n(t){}
    T n;
};

struct Print {
    Print(int m): n(m){}
    int n;
};

// Incomplete declaration of Bind to handle recurent ref to variant
template<typename T1, typename T2 = T1>
struct Bind;

// Proc
template<typename T1, typename T2 = T1>
using Proc = std::variant<Ret<T1>, Print, Bind<T1, T2>>;

// Complete decl here
template<typename T1, typename T2>
struct Bind {
    Bind(Proc<T1, T2> a, std::function<Proc<T2>(T1)> &cb): in(a), f(cb) {}
    Proc<T1, T2> &in;
    std::function<Proc<T2>(T1)> &f;
};

// Visit Proc
template<typename T1, typename T2 = T1>
void visit_ind(Proc<T1, T2>& s) {
    std::visit(overloaded {
	    [](auto arg) { std::cout << arg << ' '; },
	    [](Print& arg) { std::cout << "Print: " << arg.n << std::endl; },
		// succeeds even tho T n does not guarantee `<<`; typecheker is weak
	    [](Ret<T1>& arg) { std::cout << "Ret<T> " << arg.n << std::endl; },
	    [](Bind<T1, T2>& arg) { std::cout << "Bind<T1, T2>" << std::endl; },
    }, s);
}

// --------------- Main ---------------
int main() {
  	Proc<int> foo = Ret(42);
  	Proc<int> bar = Print(666);
	visit_ind(foo);
	visit_ind(bar);
	return 0;
}
