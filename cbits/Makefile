all: libclangjson.so main

libclangjson.so: clangjson.cpp
	clang++ $< -shared -fPIC -Wl,-undefined -Wl,dynamic_lookup -lclang -I/usr/local/lib/llvm-3.8/include -L/usr/local/lib/llvm-3.8/lib -o $@
	cp libclangjson.so ..

main: main.cpp clangjson.cpp
	clang++ -std=c++17 $< -lc++experimental -lclang -I/usr/local/lib/llvm-6.0/include -L/usr/local/lib/llvm-6.0/lib -o main
