#!/bin/sh
clang++ main.cpp -lclang -I/usr/local/lib/llvm-3.8/include -L/usr/local/lib/llvm-3.8/lib -o parser
