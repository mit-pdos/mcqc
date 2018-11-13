#include <iostream>
#include "Ind.cpp"

/*
Inductive List {T} :=
  | NIL: @List T
  | CONS: T -> @List T -> @List T.
*/

// Generate show methods by Generics maybe
template<typename T>
void print(std::shared_ptr<List<T>> l) {
    match(l,
        []() { std::cout << "null" << std::endl; },
        [](T t, std::shared_ptr<List<T>> tail){ std::cout << t << " "; print(tail); });
}

int main() {
    auto l = cons(3, cons(4, nil<int>()));
    print(l);
    return 0;
}
