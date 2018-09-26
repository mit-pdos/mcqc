#ifndef PROC_H
#define PROC_H
#include <iostream>
#include <future>
#include "string.hpp"
#include "list.hpp"
#include "exception.h"
#include "optional.hpp"
#include "nat.hpp"
#include "tuple.hpp"

// Keep libc from poluting the global namespace
namespace sys {
    #include <fcntl.h>
    #include <unistd.h>
}

using namespace string;

namespace proc {

    // Make proc an alias for the enclosing type
    template<class T>
    using Proc = T;

    // Filedescriptor type
    using Fd = nat::Nat;

    // open file
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static inline nat::Nat open(S&& s) noexcept(false) {
        if (int o = sys::open(FWD(s).c_str(), O_RDWR | O_CREAT | O_EXLOCK)) {
            return static_cast<nat::Nat>(o);
        }
        throw IOException("File not found");
    }

    // read file
    static inline String read(nat::Nat fd, nat::Nat size) {
        auto dp = string::String(size, '\0' );
        sys::read(fd, &(dp[0]), sizeof(char)*size);
        return dp;
    }

    // write file
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static inline void write(nat::Nat fd, S&& s) {
        sys::write(fd, &s[0], sizeof(char)*s.size());
    }

    // close file
    static inline void close(nat::Nat f) noexcept(false) {
        if(sys::close(f)) {
            throw IOException("Could not close file");
        }
    }

    // Create hardlink
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static inline Bool link(S&& src, S&& dest) {
        if(sys::link(FWD(src.c_str()), FWD(dest.c_str()))) {
            return true;
        }
        return false;
    }

    // Remove link from directory
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static inline Bool unlink(S&& path) {
        if(sys::unlink(FWD(path.c_str()))) {
            return true;
        }
        return false;
    }

    // until: Loop by closure passing
    // until :: (T -> B) -> (optional T -> T) -> option T -> T
    template<typename FuncCmp, typename Func, typename T,
             typename = std::enable_if_t<CallableWith<FuncCmp, T>
                && "1st argument func not callable with T">,
             typename = std::enable_if_t<CallableWith<Func, optional::Optional<T>>
                && "2nd argument func not callable with Optional<T>">,
             typename = std::enable_if_t<std::is_same_v<bool, std::invoke_result_t<FuncCmp, T>>
                && "Return type of FuncCmp is not bool">,
             typename = std::enable_if_t<std::is_same_v<T, std::invoke_result_t<Func, optional::Optional<T>>>
                && "Return type of Func is not T">>
    static inline T until(FuncCmp fcmp, Func f, optional::Optional<T> init) {
        optional::Optional<T> base = init;
        T result;
        do {
            result = f(base);
            base = optional::some(result);

        } while (fcmp(result));
        return result;
    };

    // random number
    static inline nat::Nat random() {
        return rand();
    }

    // Spawn an async process
    // TODO: Wait on return type
    template<typename Func,
             typename ...Args,
             typename = std::enable_if_t<CallableWith<Func, Args...> && "Argument not callable with argument types">>
    static inline void spawn(Func f, Args... args) {
		std::async(std::launch::async, f, args...);
    }

    // print Nat
    static inline void printn(nat::Nat n) {
        std::cout << n << std::endl;
    }

    // print string to standard output
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static void print(S&& s) {
        std::cout << s << std::endl;
    }
}
#endif
