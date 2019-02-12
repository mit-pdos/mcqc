#ifndef VARIANT_H
#define VARIANT_H
#include <variant>
#include <memory>

namespace Variant {

    // Overload functions to a single overloaded definition
    template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

    // Match boilerplate
    template<typename T, typename ...Args>
    inline auto gmatch(std::shared_ptr<T> m, Args... args) {
        return std::visit(overloaded { args... }, *m);
    }
}
#endif
