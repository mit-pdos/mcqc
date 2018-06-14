#include "string.hpp"
#include <iostream>

namespace proc {

	using Fd=FILE *;

	// open file
	Fd open(string::String s) {
		return fopen(s.c_str(), "r");
	}

	// read file
	string::String read(Fd f) {
		fseek(f, 0, SEEK_END);
		long fsize = ftell(f);
		fseek(f, 0, SEEK_SET);  //same as rewind(f);
		string::String dp = string::String(fsize, '\0' );
		fread(&(dp[0]), sizeof(char), (size_t)fsize, f);
		return dp;
	}

	// write file
	void write(Fd f, string::String& s) {
        fwrite(&s[0], sizeof(char), s.size(), file);
	}

	// close file
	void close(Fd f) {
		fclose(f);
	}

	// print to standard output
	void print(string::String& s) {
		std::cout << s << std::endl;
	}

    // forever: infinite loop
	template<typename Func>
    void forever(Func inner) {
		static_assert(std::is_function<typename std::remove_pointer<Func>::type>::value, "Argument needs to be a lambda");
        bool retval = inner();
        while(retval) {
            // Maybe need to pass arguments here?
            retval = inner();
        }
    };
}
