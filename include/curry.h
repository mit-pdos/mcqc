#ifndef CURRY_H
#define CURRY_H
#include <type_traits>

template< class, class = std::void_t<> > struct
needs_unapply : std::true_type { };

template< class T > struct
needs_unapply<T, std::void_t<decltype(std::declval<T>()())>> : std::false_type { };

template <typename F> auto
curry(F&& f);

template <bool> struct
curry_on;

template <> struct
curry_on<false> {
    template <typename F>
	static auto apply(F&& f) {
        return f();
    }
};

template <> struct
curry_on<true> {
    template <typename F>
	static auto apply(F&& f) {
        return [=](auto&& x) {
            return curry(
                [=](auto&&...xs) -> decltype(f(x,xs...)) {
                    return f(x,xs...);
                }
            );
        };
    }
};

template <typename F>
auto curry(F&& f) {
    return curry_on<needs_unapply<decltype(f)>::value>::template apply(f);
}

#endif
