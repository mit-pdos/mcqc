#include <iostream>
#include <cstring>

static void print(const char* s) {
	std::cout << s << std::endl;
}

static int count(const char* s) {
	return strlen(s);
}

static void printnum(int a) {
	std::cout << a << std::endl;
}

template<typename Func, typename Func1>
static void inline compose_void(Func1 vf, Func f) {
    vf(); // returns void
    f(); // takes in void
}

template<typename Func, typename Func1>
static void inline compose(Func1 g, Func f) {
    auto r = g(); // returns void
    f(r); // takes in void
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

    // Nest three things that return
    compose([]() { return count("hello world"); }, [](int c) {
        compose([=]() { return count("hello again"); }, [](int b) {
            compose_void([=]() { printnum(b); }, []() {
                return;
            });
        });
    });


    return 0;
}

