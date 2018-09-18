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
    // TODO: More strict typechecking
    template<typename FuncCmp, typename Func, typename T>
    static inline T until(FuncCmp fcmp, Func f, optional::Optional<T> init) {
        // static_assert(std::is_function<typename std::remove_pointer<FuncCmp>::type>::value, "Compare func needs to be a lambda");
        // static_assert(std::is_function<typename std::remove_pointer<Func>::type>::value, "Argument func needs to be a lambda");
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
    static void prints(S&& s) {
        std::cout << s << std::endl;
    }

    // print list to standard output, cannot use universal reference as print(T&&) is defined twice
    template<typename T>
    static void printl(list::List<T> l) {
        std::cout << "{";
        for(auto i = l.begin(); i != l.end(); ++i) {
            if (i != l.begin())
                std::cout << ", ";
            std::cout << *i;
        }
        std::cout << "}" << std::endl;
    }

    // print optional to standard output
    template<typename T>
    static void printo(optional::Optional<T> o) {
        match(o,
            []() { std::cout << "None" << std::endl; },
            [](T c) { std::cout << "Some " << c << std::endl; });
     }

     template<class TupType, size_t... I>
     static void printt(const TupType& t, std::index_sequence<I...>)
     {
         std::cout << "(";
         (..., (std::cout << (I == 0? "" : ", ") << std::get<I>(t)));
         std::cout << ")\n";
     }

     template<class ...Args>
     static void printt(const std::tuple<Args...>& t)
     {
         printt(t, std::make_index_sequence<sizeof...(Args)>());
     }

}
#endif
