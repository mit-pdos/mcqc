#include <iostream>
#include "List.cpp"

// Generate show methods by Generics maybe
template<typename T>
void print(std::shared_ptr<coq_List<T>> l) {
    match(l,
        []() { std::cout << "null" << std::endl; },
        [](T t, std::shared_ptr<coq_List<T>> tail){ std::cout << t << " "; print(tail); });
}

int main() {
    auto l = cons(3, cons(4, nil<int>()));
    print(l);
    return 0;
}
