#define XTL_DEFAULT_SYNTAX 'S'

#include <mach7/match.hpp>                 // Support for Match statement
#include <mach7/patterns/constructor.hpp>  // Support for constructor patterns
#include <mach7/patterns/n+k.hpp>          // Support for n+k patterns

#include <math.h>
#include <iostream>

using namespace mch; // Mach7's library namespace

//------------------------------------------------------------------------------

int fib(int n)
{
    var<int> m;

    Match(n)
    {
      When(0)     return 1;
      When(1)     return 1;
      When(m+2)   return fib(m) + fib(m+1);
    }
    EndMatch

    XTL_UNREACHABLE; // To avoid warning that control may reach end of a non-void function
}

#define NLIMIT 42
int main()
{
    for (int i = 0; i < NLIMIT; ++i)
        std::cout << "fib(" << i << ")=" << fib(i) << std::endl;
    return 0;
}

