#define XTL_DEFAULT_SYNTAX 'S'

#include <mach7/match.hpp>                 // Support for Match statement
#include <mach7/patterns/constructor.hpp>  // Support for constructor patterns
#include <mach7/patterns/n+k.hpp>          // Support for n+k patterns

#include <math.h>
#include <iostream>

using namespace mch; // Mach7's library namespace

#define ZERO C<O>()
#define ONE C<S>(C<O>())
#define TWO C<S>(C<S>(C<O>()))
#define THREE C<S>(C<S>(C<S>(C<O>())))
#define FOUR C<S>(C<S>(C<S>(C<S>(C<O>()))))

//------------------------------------------------------------------------------

// Declare C++ equivalent of an Algebraic Nat type
struct Nat      { virtual ~Nat() {} };
struct O : Nat  {};
struct S : Nat  { Nat& n; };

// Tell Mach7 library which members should be bound in which binding positions
namespace mch
{
    template <> struct bindings<O>   {};
    template <> struct bindings<S> { Members(S::n); };
}

// Add two inductively defined Nat
Nat* IPlus(Nat *n, Nat *m) {
	var<const Nat&> a;
	var<const Nat&> b;

    Match(*n, *m)
    {

      Case(a, ZERO)         return a; // m + 0 = m
      Case(ZERO, a)         return a; // 0 + m = m
      Case(C<S>(a), C<S>(b))  return C<S>(C<S>(IPlus(a, b))); // S n + S m = S (S (n + m))
      Otherwise() 		   cerr << "error"; return nullptr ;
    }
    EndMatch

    XTL_UNREACHABLE; // To avoid warning that control may reach end of a non-void function
}

// Fibonacci of Nat
Nat* IFib(Nat* n)
{
    var<const Nat&> m;

    Match(*n)
    {
      Case(ZERO)         return C<S>(C<O>()); // S 0 = 1
      Case(ONE)          return C<S>(C<O>()); // S 0 = 1
      Case(C<S>(m))      return IPlus(IFib(m), IFib(C<S>(m)));
      Otherwise() 		 cerr << "error"; return nullptr ;
    }
    EndMatch

    XTL_UNREACHABLE; // To avoid warning that control may reach end of a non-void function
}

// Inductive to Bitvector type
int INat2int(Nat *n) {
    var<const Nat&> m;

    Match(*n)
    {
      Case(ZERO)           return 0;
      Case(C<S>(m))        return 1 + INat2int(m);
      Otherwise() 		   cerr << "error"; return nullptr ;
    }
    EndMatch

    XTL_UNREACHABLE; // To avoid warning that control may reach end of a non-void function
}


int main()
{

    std::cout << "fib(" << 4 << ")=" << fib(4) << std::endl;

	// Compute and cast to bitvector int
	int result = INat2int(IFib(FOUR));
    std::cout << "fib_ind(" << 4 << ")=" << result << std::endl;
    return 0;
}

