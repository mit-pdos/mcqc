#include <iomanip>
#include <iostream>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

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
    template <typename F> static auto
    apply(F&& f) {
        return f();
    }
};

template <> struct
curry_on<true> {
    template <typename F> static auto
    apply(F&& f) {
        return [=](auto&& x) {
            return curry(
                [=](auto&&...xs) -> decltype(f(x,xs...)) {
                    return f(x,xs...);
                }
            );
        };
    }
};

template <typename F> auto
curry(F&& f) {
    return curry_on<needs_unapply<decltype(f)>::value>::template apply(f);
}

int main()
{
  auto f = [](auto a, auto b, auto c, auto d) {
    return a  * b * c * d;
  };

  return curry(f)(1)(2)(3)(4);
}
