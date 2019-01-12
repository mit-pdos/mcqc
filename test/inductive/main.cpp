#include <iostream>
#include "List.cpp"

// Generate show methods by Generics maybe
template<typename T>
void print(std::shared_ptr<list<T>> l) {
    match(l,
        []() { std::cout << "null" << std::endl; },
        [](T t, std::shared_ptr<list<T>> tail){ std::cout << t << " "; print(tail); });
}

int main() {
    auto l = coq_cons(3, coq_cons(4, coq_nil<int>()));
    print(l);
    return 0;
}
