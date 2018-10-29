#include <string>
#include <cassert>
#include <iostream>

/*
Inductive List {T} :=
  | NIL: @List T
  | CONS: T -> @List T -> @List T.
*/

template<typename T>
using Ptr = std::shared_ptr<T>;

// List implementation here
enum ListCtor {
  NIL,
  CONS
};

template<typename T>
struct list {
  enum ListCtor type;

  struct Nil {};
  struct Cons {
    T h;
    Ptr<list<T>> ts;
    Cons(T a, Ptr<list<T>> b): h(a) { ts = b; };
  };

  union {
    Nil n;
    Cons c;
  };

  ~list() {}

  list():type(NIL) { n = Nil(); };
  list(T a, Ptr<list<T>> l):type(CONS) { c = Cons(a, l); };
};

template<typename T>
Ptr<list<T>> nil() {
    return std::make_shared<list<T>>();
}

template<typename T>
Ptr<list<T>> cons(T a, Ptr<list<T>> b) {
    return std::make_shared<list<T>>(a, b);
}

template<typename T, typename Func, typename Func2>
auto match(Ptr<list<T>> l, Func f, Func2 g) {
    switch (l->type) {
        case NIL: return f();
        case CONS: return g(l->c.h, l->c.ts);
    }
}

// Generate show methods by Generics maybe
template<typename T>
void print(Ptr<list<T>> l) {
    match(l,
        []() { std::cout << "null" << std::endl; },
        [](T t, Ptr<list<T>> tail){ std::cout << t << " "; print(tail); });
}

int main() {
    auto l = cons(3, cons(4, nil<int>()));
    print(l);
    return 0;
}
