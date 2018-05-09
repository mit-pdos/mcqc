#include<iostream>

using namespace std;

/*
Inductive proc: Type -> Type :=
| ret: forall T, T -> proc T
| bind: forall T T', proc T -> (T -> proc T') -> proc T'
| print: nat -> proc unit.

Fixpoint f(a: nat) :=
  match a with
    | 0 => ret tt
    | S n => bind (print a) (fun _ => f n)
  end.
*/

template <unsigned int n>
struct fib {
	enum { value = fib<n-1>::value + fib<n-2>::value };
};

template <>
struct fib<0> {
	enum { value = 1 };
};

template <>
struct fib<1> {
	enum { value = 1 };
};


// Usage examples:
// factorial<0>::value would yield 1;
// factorial<4>::value would yield 24.

int main() {
  cout << fib<30>::value << endl;
}
