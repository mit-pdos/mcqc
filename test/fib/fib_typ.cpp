#define XTL_DEFAULT_SYNTAX 'S'

#include <iostream>
#include <mach7/type_switchN-patterns.hpp> // Support for N-ary Match statement on patterns
#include <mach7/patterns/address.hpp>      // Address and dereference combinators
#include <mach7/patterns/bindings.hpp>     // Mach7 support for bindings on arbitrary UDT
#include <mach7/patterns/constructor.hpp>  // Support for constructor patterns
#include <mach7/patterns/primitive.hpp>    // Wildcard, variable and value patterns
#include <xtl/adapters/std/memory.hpp>     // XTL subtyping adapters for standard smart pointers

using namespace mch;

//------------------------------------------------------------------------------

struct Nat
{
    virtual ~Nat() {}
};

struct O : Nat
{
};

struct S : Nat
{
    S(const Nat* n) : e(n) {}
   ~S() { if (e) delete e; }
    const Nat* e;
};

//------------------------------------------------------------------------------

namespace mch ///< Mach7 library namespace
{
template <> struct bindings<O> { };
template <> struct bindings<S> { Members(S::e); };
} // of namespace mch

//------------------------------------------------------------------------------

int evl(const Nat& n)
{
    var<const Nat&> e;

    Match(n)
    {
        Case(C<O>()  ) return 0;
        Case(C<S>(&e)) return evl(e) + 1;
    }
    EndMatch

    XTL_UNREACHABLE; // To avoid warning that control may reach end of a non-void function
}

//------------------------------------------------------------------------------

// Add two inductively defined Nat
const Nat* plus(const Nat* n, const Nat* m)
{
	var<const Nat*> a;
	var<const Nat*> b;

    Match(n, m)
    {

      Case(_, C<O>())        return n; // m + 0 = m
      Case(C<O>(), _)        return m; // 0 + m = m
      Case(C<S>(a), C<S>(b)) return new S(new S(plus(a, b))); // S n + S m = S (S (n + m))
      Otherwise()            return nullptr;
    }
    EndMatch

    XTL_UNREACHABLE; // To avoid warning that control may reach end of a non-void function
}

// Fibonacci of Nat
Nat* fib(Nat* n)
{
    var<const Nat*> m;

    Match(*n)
    {
      Case(C<O>())         return new S(new O()); // S 0 = 1
      Case(C<S>(C<O>()))   return new S(new O()); // S 0 = 1
      Case(C<S>(C<S>(m)))  return plus(fib(m), fib(new S(m)));
      Otherwise() 		   std::cerr << "error"; return nullptr ;
    }
    EndMatch

    XTL_UNREACHABLE; // To avoid warning that control may reach end of a non-void function
}

int main()
{
    Nat* a = new S(new S(new O));
    Nat* b = new S(new O);

    std::cout << evl(*a) << std::endl;
    std::cout << evl(*b) << std::endl;

    const Nat* c = plus(a,b);

    std::cout << evl(*c) << std::endl;
}

