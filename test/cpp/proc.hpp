#include <string>
#include <cstdio>
#include "terms.hpp"

using namespace func;

struct Proc {
    // print function
    const Term<unit, int> print = Term<unit, int>(
        [](int a) {
            printf("%d", a);
        });
    // open function
    const Term<FILE *, std::string> open= Term<FILE *, std::string>(
        [](std::string filename) {
            return fopen(filename.c_str(), "rw");
        });
    // close function
    const Term<unit, FILE *> close = Term<unit, FILE *>(
        [](FILE * file) {
            fclose(file);
        });
    // read function
    const Term <std::string, FILE *, int> read = Term<std::string, FILE *, int>(
        [](FILE * file, int strsize) {
            // Warning: Unsafe to overloads and smaller files than `strsize`
            std::string data(strsize, '\0' );
            fread(&data[0], sizeof(char), (size_t)strsize, file);
            // Use smart pointers for GC
            return data;
        });
    // write function
    const Term <unit, FILE *, std::string> write = Term<unit, FILE *, std::string>(
        [](FILE * file, std::string data) {
            // Warning: Unsafe to overloads
            fwrite(&data[0], sizeof(char), data.size(), file);
        });
    // forever: infinite loop
    const Term <unit, Term<unit, unit>> forever = Term<unit, Term<unit, unit>>(
        [](Term<unit, unit>&& inner) {
            while(1) {
                // Maybe need to pass arguments here?
                inner();
            }
        });
};
