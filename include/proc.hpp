#ifndef PROC_H
#define PROC_H
#include <iostream>
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

    // print Nat
    static inline void print(nat::Nat n) {
        std::cout << n << std::endl;
    }

    // print string to standard output
    template<typename S=String, typename = std::enable_if_t<is_same_kind_v<String, S>>>
    static void print(S&& s) {
        std::cout << s << std::endl;
    }

    // print list to standard output, cannot use universal reference as print(T&&) is defined twice
    template<typename T>
    static void print(list::List<T> l) {
        std::cout << "{ ";
        for(auto i = FWD(l).begin(); i != FWD(l).end(); ++i) {
            if (i != FWD(l).begin())
                std::cout << ", ";
            std::cout << *i;
        }
        std::cout << "}" << std::endl;
    }

    // print optional to standard output
    template<typename T>
    static void print(optional::Optional<T> o) {
        match(FWD(o),
            []() { std::cout << "None" << std::endl; },
            [](T c) { std::cout << "Some " << c << std::endl; });
     }

     template<class TupType, size_t... I>
     static void print(const TupType& t, std::index_sequence<I...>)
     {
         std::cout << "(";
         (..., (std::cout << (I == 0? "" : ", ") << std::get<I>(t)));
         std::cout << ")\n";
     }
     
     template<class ...Args>
     static void print (const std::tuple<Args...>& t)
     {
         print(t, std::make_index_sequence<sizeof...(Args)>());
     }

}
#endif
