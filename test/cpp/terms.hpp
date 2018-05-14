#include <functional>
#include <variant>

#ifndef TERMS_HPP
#define TERMS_HPP

namespace func {
    // Declare unit type
    typedef std::monostate unit;

    // Empty type, maybe use this instead of monostate
    template<typename...>
    struct Term;

    // Variables
    template<typename T>
    struct Term<T>
    {
        T val;
        // Constructor
        Term(T in): val(in) {}

        // Call op just returns the value
        T operator() () const {
            return this->val;
        }
    };

    // Functions that return void
    template <typename T, typename ... Args>
    struct Term<T, Args...>
    {
        using R = typename std::conditional<std::is_same<T, unit>::value,void,T>::type;
        // Void returning function
        const std::function<R(Args...)> body;

        // Void returning constructor
        Term(std::function<R(Args...)> func): body(func) {}

        // Void function Caller
        template <typename U = R>
        typename std::enable_if<std::is_same<U, unit>::value,void>::type
        operator() (Args&& ...a) const {
            this->body(std::forward<Args>(a)...);
        }

        // T returning function Caller
        template <typename U = R>
        typename std::enable_if<!std::is_same<U, unit>::value,T>::type
        operator() (Args&& ...a) const {
            return this->body(std::forward<Args>(a)...);
        }

    };
}

#endif
