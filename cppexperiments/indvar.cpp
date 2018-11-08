#include <iostream>
#include <variant>

/*
Inductive List {T} :=
  | NIL: @List T
  | CONS: T -> @List T -> @List T.
*/

// Overload functions to a single overloaded definition
template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

struct Nil {};
template<typename T>
struct Cons {
    T a;
    std::shared_ptr<std::variant<Nil, Cons<T>>> b;
    Cons(T a, std::shared_ptr<std::variant<Nil, Cons<T>>> b) { this->a = a; this->b = b; };
};
template<typename T>
using list = std::variant<Nil, Cons<T>>;
template<typename T>
std::shared_ptr<list<T>> nil() {
    return std::make_shared<list<T>>(Nil());
}
template<typename T>
std::shared_ptr<list<T>> cons(T a, std::shared_ptr<list<T>> b) {
    return std::make_shared<list<T>>(Cons(a,b));
}
template<typename T, typename Func, typename Func2>
auto match(std::shared_ptr<list<T>> l, Func f, Func2 g) {
    return std::visit(overloaded {
            [&](Nil a) { return f(); },
            [&](Cons<T> b) { return g(b.a, b.b); }
           }, *l);
}

// Generate show methods by Generics maybe
template<typename T>
void print(std::shared_ptr<list<T>> l) {
    match(l,
        []() { std::cout << "null" << std::endl; },
        [](T t, std::shared_ptr<list<T>> tail){ std::cout << t << " "; print(tail); });
}

int main() {
    auto l = cons(3, cons(4, nil<int>()));
    print(l);
    return 0;
}
