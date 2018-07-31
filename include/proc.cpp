#ifndef PROC_CPP
#define PROC_CPP
#include "string.cpp"
#include "list.cpp"
#include <iostream>

namespace proc {

	using Fd=FILE *;
	using proc=void;

	// open file
	static inline Fd open(string::String s) {
		return fopen(s.c_str(), "r");
	}

	// read file
	static inline string::String read(Fd f) {
		fseek(f, 0, SEEK_END);
		long fsize = ftell(f);
		fseek(f, 0, SEEK_SET);  //same as rewind(f);
		string::String dp = string::String(fsize, '\0' );
		fread(&(dp[0]), sizeof(char), (size_t)fsize, f);
		return dp;
	}

	// write file
	static inline void write(Fd f, string::String& s) {
        fwrite(&s[0], sizeof(char), s.size(), f);
	}

	// close file
	static inline void close(Fd f) {
		fclose(f);
	}

	// print string to standard output
	static inline void print(string::String& s) {
		std::cout << s << std::endl;
	}

	// print list to standard output
	template<typename T>
	static inline void print(const list::List<T>& s) {
		std::cout << "{ ";
		for(typename list::List<T>::const_iterator i = s.begin(); i != s.end(); ++i) {
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
