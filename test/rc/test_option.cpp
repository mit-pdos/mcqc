// RUN: %crc %s -o %t.exe
// RUN: %t.exe > FileCheck %s
// CHECK:      OK, passed 100 tests
// CHECK-NEXT: OK, passed 100 tests
#include <rapidcheck.h>

using namespace rc;

// Ours
#include <option.hpp>
#include <nat.hpp>
#include <type_checks.h>

using namespace Option;
using namespace Nat;

void check_option_match() {
  rc::check("Option<nat> match", [](const bool& b, const nat& n) {
    option<nat> o;
    // Check both none() and some with this
    if (b) {
        o = some(const_cast<nat&>(n));
    } else {
        o = none<nat>();
    }
    return match(o,
      [=](auto &&val){ return o == some(n) && b; },
      [=]() { return o == none<nat>() && !b; });
  });

  rc::check("Option<option<nat>> match", [](const bool& b, const nat& n) {
    option<option<nat>> o;
    // Check both none() and some with this
    if (b) {
        o = some(some(const_cast<nat&>(n)));
    } else {
        o = none<option<nat>>();
    }
    return match(o,
      [=](auto &&val){ return val == some(n) && b; },
      [=]() { return o == none<option<nat>>() && !b; });
  });

}

int main() {
  check_option_match();
  return 0;
}
