#include <string>
#include <cstdio>
#include <functional>
#include <memory>

namespace proc {
    // print function
    inline void print(int a) {
        printf("%d\n", a);
    };
    // open function
    inline FILE* open(std::string &filename) {
    	return fopen(filename.c_str(), "rw");
    };
    // close function
    inline void close(FILE * file) {
        fclose(file);
    };
    // read function
    std::unique_ptr<std::string> read(FILE * file, int strsize) {
		// Warning: Unsafe to overloads and smaller files than `strsize`
		std::unique_ptr<std::string> dp = std::make_unique<std::string>(strsize, '\0' );
		fread(&(*dp)[0], sizeof(char), (size_t)strsize, file);
		// Use smart pointers for GC
		return dp;
	};
    // write function
    void write(FILE * file, std::string data) {
		// Warning: Unsafe to overloads
		fwrite(&data[0], sizeof(char), data.size(), file);
	};
    // forever: infinite loop
    void forever(std::function<bool(void)>& inner) {
		bool retval = inner();
        while(retval) {
        	// Maybe need to pass arguments here?
            retval = inner();
        }
    };
};
