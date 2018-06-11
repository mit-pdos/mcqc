#include <iostream>
#include <cstring>

static void print(const char* s) {
	std::cout << s << std::endl;
}

static int count(const char* s) {
	return strlen(s);
}

static void printnum(int a) {
	std::cout << "A number: " << a << std::endl;
}

template<typename Func, typename Func1>
static void inline compose_void(Func1 vf, Func f) {
    vf(); // returns void
    f(); // takes in void
}

template<typename Func, typename Func1>
static void inline compose(Func1 g, Func f) {
    auto r = g(); // returns auto
    f(r); // takes in auto
}

template<typename Func>
static void inline forever(Func f) {
	auto r = f();
	while(r) {
		r = f();
	}
}
// --------------- Main ---------------
int main() {
    // Nest three things
    compose_void([]() { print("hello world"); }, []() {
        compose_void([]() { print("hello again"); }, []() {
            compose_void([]() { print("hello third time"); }, []() {
                return;
            });
        });
    });

    // Nest three things (two return)
    compose([]() { return count("hello world"); }, [](int c) {
        compose([]() { return count("hello again"); }, [](int b) {
            compose_void([=]() { printnum(b); }, []() {
                return;
            });
        });
    });

	// Forever loop
	forever([]() {
		std:: cout << "I print once!" << std::endl;
		return false;
	});

    return 0;
}

