#include <iostream>
#include "type_deduction.hpp"
#include <functional>
#include <variant>

template<class T> struct always_false : std::false_type {};

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

// Attempt at inductive monad definition
template<typename T>
struct Proc   { virtual ~Proc<T>() {} };

template<typename T>
struct Ret   : Proc<T> {
    Ret(T t) : n(t){}
    T n;
};

template<typename T1, typename T2>
struct Bind  : Proc<T2> {
    Bind(Proc<T1> a, std::function<Proc<T2>(T1)> &cb): in(a), f(cb) {}
    Proc<T1> &in;
    std::function<Proc<T2>(T1)> &f;
};

struct Print : Proc<int> {
    Print(int m): n(m){}
    int n;
	const void operator()() {
		std::cout << this->n << std::endl;
	}
};

// Play with sum types
template<typename T>
using var_t = std::variant<Ret<T>, Print>;

using var_char = var_t<char>;

// Pin types to <int, char>
// Inductive step f<a>
template<int a>
struct f {
    static const Bind<int, char> value;
};
template<int a>
inline std::function<Proc<char>(int)> lambda = [](auto _) {
    return f<a-1>::value;
};
template<int a>
const Bind<int, char> f<a>::value = Bind(Print(a), lambda<a>);

// Base case f<0>
template <>
struct f<0> {
    static const Ret<char> value;
};
const auto f<0>::value = Ret<char>('a');

// Visitor code
template<typename T2>
inline std::function<(var_char)> visit_lambda = [](auto&& arg) {
	using T = std::decay_t<decltype(arg)>;
	if constexpr (std::is_same_v<T, Print>)
		arg(); // will print
	else if constexpr (std::is_same_v<T, Ret<T2>>)
		std::cout << "Ret with type " << type_name<decltype(arg)>() << '\n';
	else
		static_assert(always_false<T>::value, "non-exhaustive visitor!");
};

int main() {
  auto foo = f<10>();
  std::cout << type_name<decltype(foo)>() << '\n';

  var_t<char> retint = Ret<char>('#');
  var_t<char> retprint = Print(42);

  // type-matching visitor: can also be a class with 4 overloaded operator()'s
  std::visit(visit_lambda, retprint);
  std::visit(visit_lambda, retint);

  return 0;
}
