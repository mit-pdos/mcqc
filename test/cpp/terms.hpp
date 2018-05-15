#include <functional>
#include <variant>

#ifndef TERMS_HPP
#define TERMS_HPP

namespace func {
    // Declare unit type
    typedef std::monostate unit;

	template<typename T>
	struct is_unit {
		static constexpr bool value = std::is_same<T, std::monostate>::value;
	};

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
        using R = typename std::conditional<is_unit<T>::value,void,T>::type;
        // Void returning function
        const std::function<R(Args...)> body;

        // Void returning constructor
        Term(std::function<R(Args...)> func): body(func) {}

        // Void function Caller
        template <typename U = R>
        typename std::enable_if_t<is_unit<U>::value,void>
        operator() (Args&& ...a) const {
            this->body(std::forward<Args>(a)...);
        }

        // T returning function Caller
        template <typename U = R>
        typename std::enable_if_t<!is_unit<U>::value,T>
        operator() (Args&& ...a) const {
            return this->body(std::forward<Args>(a)...);
        }
    };

	template <typename T, typename T1, typename ... Args>
    struct Term<T, T1, Args...>
    {
        using R = typename std::conditional<is_unit<T>::value,void,T> :: type;
        using R1 = typename std::conditional<is_unit<T1>::value,void,T> :: type;

        // Void returning function
        const std::function<R(R1, Args...)> body;

        // Void returning constructor
        Term(std::function<R(R1, Args...)> func): body(func) {}

        // void -> void function Caller
        template <typename U = R, typename U1 = R1>
        typename std::enable_if_t<is_unit<U>::value &&
									is_unit<U1>::value, void>
        operator() (Args&& ...a) const {
            this->body();
        }

        // T -> void function Caller
        template <typename U = R, typename U1 = R1>
        typename std::enable_if_t<is_unit<U>::value &&
									!is_unit<U1>::value, void>
        operator() (R1&& first, Args&& ...a) const {
            this->body(std::forward<R1>(first), std::forward<Args>(a)...);
        }

        // void -> T function Caller
        template <typename U = R, typename U1 = R1>
        typename std::enable_if_t<!is_unit<U>::value &&
									is_unit<U1>::value, R>
        operator() (R1&& first, Args&& ...a) const {
            return this->body(std::forward<Args>(a)...);
        }

        // T -> T function Caller
        template <typename U = R, typename U1 = R1>
        typename std::enable_if_t<!is_unit<U>::value &&
									!is_unit<U1>::value, R>
        operator() (R1&& first, Args&& ...a) const {
            return this->body(std::forward<R1>(first), std::forward<Args>(a)...);
        }
    };
}

#endif
