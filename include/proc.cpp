#include <iostream>
#include <cstdio>

class Proc {
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

