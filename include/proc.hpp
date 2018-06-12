#ifndef PROC_H
#define PROC_H
#include <cstdio>
#include "string.hpp"
#include "nat.hpp"

class Proc {
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

#endif
