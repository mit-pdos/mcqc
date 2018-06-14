#ifndef TYPE_CHECKS_H
#define TYPE_CHECKS_H
#include <type_traits>
#include <utility>

// Checks if type is callable by decltype magic
template<class T, class...Args>
struct is_callable
{
    template<class U> static auto test(U*p) -> decltype((*p)(std::declval<Args>()...), void(), std::true_type());

    template<class U> static auto test(...) -> decltype(std::false_type());

    static constexpr auto value = decltype(test<T>(nullptr))::value;
};

// Checks if type is callable with by more magic
template<class T, class...Args>
static constexpr auto CallableWith = is_callable<T, Args...>::value;
#endif
