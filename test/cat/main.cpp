#include <iostream>
#include "Cat.cpp"

int main(int argc, char** argv) {
    for(int i = 1; i < argc; ++i) {
		cat(".", argv[i]);
	}
    return 0;
}

