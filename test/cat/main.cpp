#include <iostream>
#include "benchmark.h"
#include "Cat.cpp"
#include "proc.cpp"

using namespace proc;

int main() {
    // Benchmark, start
    tic();
    // Read and print a file
    cat(".", "Cat.v");
    // Benchmark, stop
    toc();
    return 0;
}

