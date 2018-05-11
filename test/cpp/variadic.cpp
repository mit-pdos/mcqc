#include <iostream>

// Unit type
template<typename ... Ts>
struct Term
{
};

// Variable coq types
template<typename T>
struct Term<T>
{
	T val;
	// Constructor
	Term(T in): val(in) {}

	// Call op just returns the value
	T operator()() {
		return this->val;
	}
};

// Functions
template <typename T, typename ... Args>
struct Term<T, Args...>
{
	// Function body
	const std::function<T(Args...)> body;

	// Constructor
	Term(std::function<T(Args...)> func): body(func) {}

	// Caller
	T operator()(Args&& ...a) {
		return this->body(std::forward<Args>(a)...);
	}
};


int main() {
	auto c = Term<int>(42);
	auto fun = Term<int, int, int>([](int a, int b) { return a + b; });

	std::cout << c() << std::endl;
	std::cout << fun(3,4) << std::endl;

	return 0;
}
