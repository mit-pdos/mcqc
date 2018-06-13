#ifndef OPTIONAL_H
#define OPTIONAL_H
#include <tuple>
#include <optional>

namespace optional {
    // Nat definition
    enum OptionalAtom { None, Some };

    template<typename T>
    using Optional = std::optional<T>;

    template<typename T>
    using OptionalTuple = const std::tuple<OptionalAtom, T>;

    template<typename T>
    constexpr const OptionalTuple<T> match(Optional<T>& o) {
        switch(o.has_value()) {
        case true:  return {Some, o.value()};
        case false: return {None, T()};
        }
        // Silence warnings
        return {None, T()};
    }

    // None
    template<typename T>
    constexpr inline static Optional<T> none();
    // Constructive cons, copies t so t can be referenced again
    template<typename T>
    constexpr inline static Optional<T> some(T t);
    // Destructive cons, reuses t so it can not be referenced again
    template<typename T>
    constexpr inline static Optional<T> dsome(T t);
}
#endif
