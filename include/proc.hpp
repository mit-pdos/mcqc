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

	// close file
	void close(Fd f) {
		fclose(f);
	}

	void print(string::String s) {
		std::cout << s << std::endl;
	}
}
