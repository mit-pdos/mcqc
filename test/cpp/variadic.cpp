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
	T operator() () const {
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
	T operator() (Args&& ...a) const {
		return this->body(std::forward<Args>(a)...);
	}
};

#include <string>
#include <cstdio>
typedef FILE* fd;
typedef std::unique_ptr<std::string> string_ptr;
typedef std::function<void(void)> void_t;
#define VOID [](){}

int main() {
	// print function
	const Term<void_t, int> print = Term<void_t, int>(
		[](int a) {
			std::cout << a << std::endl;
			return VOID;
		});
	// open function
	const Term<fd, std::string> open= Term<fd, std::string>(
		[](std::string filename) {
			return fopen(filename.c_str(), "rw");
		});
	// close function
	const Term<void_t, fd> close = Term<void_t, fd>(
		[](fd file) {
			fclose(file);
			return VOID;
		});
	// read function
	const Term <string_ptr, fd, int> read = Term<string_ptr, fd, int>(
		[](fd file, int strsize) {
			// Warning: Unsafe to overloads and smaller files than `strsize`
			std::string data(strsize, '\0' );
			fread(&data[0], sizeof(char), (size_t)strsize, file);
			// Use smart pointers for GC
			return string_ptr(&data);
		});
	// write function
	const Term <void_t, fd, std::string> write = Term<void_t, fd, std::string>(
		[](fd file, std::string data) {
			// Warning: Unsafe to overloads
			fwrite(&data[0], sizeof(char), data.size(), file);
			return VOID;
		});
	// forever: infinite loop
	const Term <void_t, Term<void_t, void_t>> forever = Term<void_t, Term<void_t, void_t>>(
		[](Term<void_t, void_t>&& inner) {
			while(1) {
				// Maybe need to pass arguments here?
				inner(VOID);
			}
			return VOID;
		});

	auto c = Term<int>(42);
	auto fun = Term<int, int, int>([](int a, int b) { return a + b; });

	std::cout << c() << std::endl;
	std::cout << fun(3,4) << std::endl;

	print(12);
	return 0;
}
