#ifndef IO_H
#define IO_H
#include <iostream>
#include <future>
#include "string.hpp"
#include "option.hpp"
#include "exception.h"
#include "nat.hpp"

// Keep libc from poluting the global namespace
namespace sys {
    #include <fcntl.h>
    #include <unistd.h>
}

namespace {
     static inline void mkhexes(std::stringstream &s, nat cnt) {
        nat r = rand() % 16;
        for(int i = 0; i < cnt; ++i, r = rand() % 16) {
            if (r > 9)
                s << (char)('a'+(char)(r - 10));
            else
                s << r;
        }
    }
}

using namespace String;
using namespace Option;
using namespace Nat;

namespace Io {

    // Make IO an alias for the enclosing type
    template<class T>
    using io = T;

    // open file
    template<typename S=string, typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static io<nat> open(S&& s) noexcept(false) {
        if (int o = sys::open(FWD(s).c_str(), O_RDWR | O_CREAT)) {
            return static_cast<nat>(o);
        }
        throw IOException("File not found");
    }

    // read file
    static io<string> read(nat f, nat size) {
        auto dp = string(size, '\0' );
        sys::read(f, &(dp[0]), sizeof(char)*size);
        return dp;
    }

    // write file
    template<typename S=string, typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static io<void> write(nat nat, S&& s) {
        sys::write(nat, &s[0], sizeof(char)*s.size());
    }

    // close file
    static io<void> close(nat f) noexcept(false) {
        if(sys::close(f)) {
            throw IOException("Could not close file");
        }
    }

    // Create hardlink
    template<typename S=string, typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static io<bool> link(S&& src, S&& dest) {
        if(sys::link(FWD(src.c_str()), FWD(dest.c_str()))) {
            return true;
        }
        return false;
    }

    // Remove link from directory
    template<typename S=string, typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static io<bool> unlink(S&& path) {
        if(sys::unlink(FWD(path.c_str()))) {
            return true;
        }
        return false;
    }

    // until: Loop by closure passing
    // until :: (T -> B) -> (option T -> T) -> option T -> T
    template<typename FuncCmp, typename Func, typename T,
             typename = std::enable_if_t<CallableWith<FuncCmp, T>
                && "1st argument func not callable with T">,
             typename = std::enable_if_t<CallableWith<Func, option<T>>
                && "2nd argument func not callable with option<T>">,
             typename = std::enable_if_t<std::is_same_v<bool, std::invoke_result_t<FuncCmp, T>>
                && "Return type of FuncCmp is not bool">>
    static io<T> until(FuncCmp fcmp, Func f, option<T> init) {
        option<T> base = init;
        T result;
        do {
            result = f(base);
            base = some(result);

        } while (fcmp(result));
        return result;
    };

    // random number
    static io<nat> randnat() {
        return static_cast<nat>(rand());
    }

    // Make UUID v4-ish ie: "ea22d131-e1fc-4f95-8afc-6286d15e802e"
    static io<string> getuuid() {
        std::stringstream ss;
        mkhexes(ss, 8);
        ss << "-";
        mkhexes(ss, 4);
        ss << "-";
        mkhexes(ss, 4);
        ss << "-";
        mkhexes(ss, 4);
        ss << "-";
        mkhexes(ss, 12);

        return ss.str();
    }

    // Spawn an async IOess
    // TODO: Wait on return type
    template<typename Func,
             typename ...Args,
             typename = std::enable_if_t<CallableWith<Func, Args...> && "Argument not callable with argument types">>
    static io<void> spawn(Func f, Args... args) {
        std::async(std::launch::async, f, args...);
    }

    // print string to standard output
    template<typename S=string, typename = std::enable_if_t<is_same_kind_v<string, S>>>
    static io<void> print(S&& s) {
        std::cout << s << std::endl;
    }
}
#endif
