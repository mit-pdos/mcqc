#ifndef PROC_CPP
#define PROC_CPP
#include <iostream>
#include "string.cpp"
#include "list.cpp"
#include "exception.hpp"
#include "optional.cpp"
#include "nat.cpp"

namespace proc {

	using Fd=FILE *;
	using proc=void;

	// open file
	static inline Fd open(string::String s) noexcept(false) {
		if (Fd o = fopen(s.c_str(), "r")) {
			return o;
		}
		throw IOException("File not found");
	}

	// read file
	static inline string::String read(Fd f, nat::Nat &size) {
		auto dp = string::String(size, '\0' );
		fread(&(dp[0]), sizeof(char), (size_t)size, f);
		return dp;
	}

	// write file
	static inline void write(Fd f, string::String& s) {
        fwrite(&s[0], sizeof(char), s.size(), f);
	}

	// close file
	static inline void close(Fd f) noexcept(false) {
		if(fclose(f)) {
			throw IOException("Could not close fd");
		}
	}

	// print string to standard output
	static inline void print(string::String& s) {
		std::cout << s << std::endl;
	}

	// print list to standard output
	template<typename T>
	static inline void print(const list::List<T>& s) {
		std::cout << "{ ";
		for(auto i = s.begin(); i != s.end(); ++i) {
			if (i != s.begin())
				std::cout << ", ";
			std::cout << *i;
		}
		std::cout << "}" << std::endl;
	}

    // until: Loop by closure passing
    // TODO: More strict typechecking
	template<typename FuncCmp, typename Func, typename T>
    static inline T until(FuncCmp fcmp, Func f, optional::Optional<T> init) {
		static_assert(std::is_function<typename std::remove_pointer<FuncCmp>::type>::value, "Compare func needs to be a lambda");
		static_assert(std::is_function<typename std::remove_pointer<Func>::type>::value, "Argument func needs to be a lambda");
        optional::Optional<T> base = init;
        T result;
        do {
			result = f(base);
            base = optional::some(result);

		} while (fcmp(result));
		return result;
    };
}
#endif
