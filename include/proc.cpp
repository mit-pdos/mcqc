#ifndef PROC_CPP
#define PROC_CPP
#include "string.cpp"
#include "list.cpp"
#include "exceptions.hpp"
#include "optional.cpp"
#include <iostream>

using namespace list;
using namespace string;

namespace proc {

	using Fd=FILE *;
	using proc=void;

	// open file
	static inline Fd open(String s) noexcept(false) {
		if (Fd o = fopen(s.c_str(), "r")) {
			return o;
		}
		throw IOException("File not found");
	}

	// read file
	static inline String read(Fd f) {
		fseek(f, 0, SEEK_END);
		long fsize = ftell(f);
		fseek(f, 0, SEEK_SET);
		String dp = String(fsize, '\0' );
		fread(&(dp[0]), sizeof(char), (size_t)fsize, f);
		return dp;
	}

	// write file
	static inline void write(Fd f, String& s) {
        fwrite(&s[0], sizeof(char), s.size(), f);
	}

	// close file
	static inline void close(Fd f) noexcept(false) {
		if(fclose(f)) {
			throw IOException("Could not close fd");
		}
	}

	// print string to standard output
	static inline void print(String& s) {
		std::cout << s << std::endl;
	}

	// print list to standard output
	template<typename T>
	static inline void print(const List<T>& s) {
		std::cout << "{ ";
		for(typename List<T>::const_iterator i = s.begin(); i != s.end(); ++i) {
			if (i != s.begin())
				std::cout << ", ";
			std::cout << *i;
		}
		std::cout << "}" << std::endl;
	}

    // forever: infinite loop
	template<typename Func>
    static inline void forever(Func inner) {
		static_assert(std::is_function<typename std::remove_pointer<Func>::type>::value, "Argument needs to be a lambda");
        bool retval = inner();
        while(retval) {
            // Maybe need to pass arguments here?
            retval = inner();
        }
    };
}
#endif
