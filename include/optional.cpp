#include <iostream>
#include <optional>
#include <tuple>
#include "optional.hpp"

namespace optional {
	// None
    template<typename T>
	constexpr inline static Optional<T> none() {
		return {};
	}

    // Constructive cons, copies t so t can be referenced again
	template<typename T>
	constexpr inline static Optional<T> some(T t) {
		return Optional<T>(t);
	}

    // Destructive cons, reuses t so it can not be referenced again
	template<typename T>
	constexpr inline static Optional<T> dsome(T t) {
		return Optional<T>(std::move(t));
	}
}

