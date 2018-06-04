#include <iostream>
#include <functional>

void inline compose(const char* s, const std::function<void(const char *)> f) {
	std::cout << s << std::endl;
	f(s);
}

// --------------- Main ---------------
int main() {
	// Nest three things
	compose("hello world", [](const char *s) {
		compose("hello again", [](const char *s) {
			compose("hello third time", [](const char *s) {
				return;
			});
		});
	});

	return 0;
}
