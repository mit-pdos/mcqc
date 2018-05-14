#include <iostream>
#include <string>
#include <cstdio>
#include "terms.hpp"

namespace proc {

    // print function
    const Term<unit, int> print = Term<unit, int>(
        [](int a) {
            printf("%d", a);
        });
    // open function
    const Term<fd, std::string> open= Term<fd, std::string>(
        [](std::string filename) {
            return fopen(filename.c_str(), "rw");
        });
    // close function
    const Term<unit, fd> close = Term<unit, fd>(
        [](fd file) {
            fclose(file);
        });
    // read function
    const Term <string_ptr, fd, int> read = Term<string_ptr, fd, int>(
        [](fd file, int strsize) {
            // Warning: Unsafe to overloads and smaller files than `strsize`
            std::string data(strsize, '\0' );
            fread(&data[0], sizeof(char), (size_t)strsize, file);
            // Use smart pointers for GC
            return string_ptr(&data);
        });
    // write function
    const Term <unit, fd, std::string> write = Term<unit, fd, std::string>(
        [](fd file, std::string data) {
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
