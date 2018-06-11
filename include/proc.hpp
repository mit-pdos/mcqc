#ifndef PROC_H
#define PROC_H
#include <cstdio>
#include "monad.hpp"
#include "string.hpp"
#include "nat.hpp"

class Proc(Monad) {
	private:
		enum ProcAtom { Print, Open, Close, Read, Write };

	public:
		using Fd = FILE*;
		inline void print(int a);
		inline FILE* open(std::string &filename);
		inline void close(FILE* file);
		inline std::string read(FILE* file, int strsize);
		inline void write(FILE* file, std::string data);
};

class Proc(Monad) {
	public:
	    // print function
    	inline void print(int a) {
        	printf("%d\n", a);
    	}
    	// open function
    	inline Fd open(std::string &filename) {
        	return fopen(filename.c_str(), "rw");
    	}
    	// close function
    	inline void close(Fd file) {
        	fclose(file);
    	}
    	// read function
    	std::string read(Fd file, int strsize) {
        	// Warning: Unsafe to overloads and smaller files than `strsize`
        	std::unique_ptr<std::string> dp = std::make_unique<std::string>(strsize, '\0' );
        	fread(&(*dp)[0], sizeof(char), (size_t)strsize, file);
        	// Use smart pointers for GC
        	return dp;
    	}
    	// write function
    	void write(Fd file, std::string data) {
        	// Warning: Unsafe to overloads
        	fwrite(&data[0], sizeof(char), data.size(), file);
    	}
};

#endif
