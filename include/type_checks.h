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

// Is the type A the same as B, regardless of const and references
template <typename A, typename B>
struct is_same_kind {
	static constexpr auto value = std::is_same_v<std::remove_cv_t<std::remove_reference_t<A>>, 
                                                 std::remove_cv_t<std::remove_reference_t<B>>>;
};

template <typename A, typename B>
static constexpr auto is_same_kind_v = is_same_kind<A,B>::value;

#define FWD(a) std::forward<decltype(a)>(a)

// Checks if type is callable with by more magic
template<class T, class...Args>
static constexpr auto CallableWith = is_callable<T, Args...>::value;
#endif
