#!/bin/sh
clang++ clangjson.cpp -shared -fPIC -Wl,-undefined -Wl,dynamic_lookup -lclang -I/usr/local/lib/llvm-3.8/include -L/usr/local/lib/llvm-3.8/lib -o libclangjson.so
