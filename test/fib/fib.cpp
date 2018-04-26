#define XTL_DEFAULT_SYNTAX 'S'

#include <mach7/match.hpp>                 // Support for Match statement
#include <mach7/patterns/constructor.hpp>  // Support for constructor patterns
#include <mach7/patterns/n+k.hpp>          // Support for n+k patterns

#include <math.h>
#include <iostream>

using namespace mch; // Mach7's library namespace

/*
Coq
-----

Fixpoint fib(n: nat) :=
  match n with
    | 0 => 1
    | 1 => 1
    | S(S m as sm) => fib(m) + fib(sm)
  end.


JSON
-----

Module {name = "Fib", need_magic = False, need_dummy = False, used_modules = Just ["Datatypes","Nat"],
	declarations =
		[FixDecl {fixlist =
			[Fix {name = Just "fib", typ = TypArrow {left = TypGlob {name = "Datatypes.nat"}, right = TypGlob {name = "Datatypes.nat"}}, value =
				ExprLambda {argnames = Just ["n"], body =
					ExprCase {expr = ExprRel {name = "n"},
						cases =
							[Case {pat = Constructor {name = "Datatypes.O", argtypes = Nothing, argnames = Just []}, body =
								ExprConstructor {name = "Datatypes.S", args = [ExprConstructor {name = "Datatypes.O", args = []}]}},
							 Case {pat = Constructor {name = "Datatypes.S", argtypes = Nothing, argnames = Just ["sm"]}, body =
								ExprCase {expr = ExprRel {name = "sm"}, cases =
									[Case {pat = Constructor {name = "Datatypes.O", argtypes = Nothing, argnames = Just []}, body =
										ExprConstructor {name = "Datatypes.S", args =
											[ExprConstructor {name = "Datatypes.O", args = []}]}},
									Case {pat = Constructor {name = "Datatypes.S", argtypes = Nothing, argnames = Just ["m"]}, body =
										ExprApply {func = ExprGlobal {name = "Nat.add"}}}]}}]}}}
             ]
		}
	]
}
*/

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

