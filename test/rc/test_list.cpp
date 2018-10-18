// RUN: %crc %s -o %t.exe
// RUN: %t.exe > FileCheck %s
// CHECK:      OK, passed 100 tests
// CHECK-NEXT: OK, passed 100 tests
// CHECK-NEXT: OK, passed 100 tests
// CHECK-NEXT: OK, passed 100 tests
// CHECK-NEXT: OK, passed 100 tests
// CHECK-NEXT: OK, passed 100 tests
#include <rapidcheck.h>

using namespace rc;

// Ours
#include <list.hpp>
#include <string.hpp>
#include <copy.hpp>
#include <nat.hpp>
#include <type_checks.h>

using namespace Nat;
using namespace List;
using namespace String;
using namespace Copy;

void check_list_match() {
  // Match and cons cancel out
  rc::check("List match & cons", [](const list<string>& l) {
    list<string>& ll = const_cast<list<string>&>(l);
    return match(copy(ll),
      [=](){ return empty(ll); },
      [=](auto h, list<string> ts) { return cons(h, ts) == ll; });
  });

  // Match and tail cancel out
  rc::check("List match & tail", [](const list<string>& l) {
    list<string>& ll = const_cast<list<string>&>(l);
    return match(copy(ll),
      [=](){ return empty(ll); },
      [=](auto h, list<string> ts) { return ts == tail(const_cast<list<string>&>(ll)); });
  });

  // Match and app cancel out
  rc::check("List match & app", [](const list<string>& l) {
    list<string>& ll = const_cast<list<string>&>(l);
    return match(copy(ll),
      [=](){ return empty(ll); },
      [=](auto h, list<string> ts) { return app(list<string>{ h }, ts) == ll; });
  });

  // Match and app cancel out
  rc::check("List match & app with fwd", [](const list<string>& l) {
    list<string>& ll = const_cast<list<string>&>(l);
    return match(copy(ll),
      [=](){ return empty(ll); },
      [=](auto h, list<string>&& ts) { return app(list<string>{ h }, FWD(ts)) == FWD(const_cast<list<string>&>(ll)); });
  });

  // Match, cons and app cancel out
  rc::check("List match, then cons & app", [](const list<string>& l) {
    list<string>& ll = const_cast<list<string>&>(l);
    return match(copy(ll),
      [=](){ return empty(ll); },
      [=](auto h, list<string> ts) { return app(list<string>{ h }, FWD(copy(ts))) == cons(h, FWD(ts)); });
  });

  // Match, head
  /*
  rc::check("List match head", [](const list<nat>& l) {
    list<nat>& ll = const_cast<list<nat>&>(l);
    return match(copy(ll),
      [=](){ return empty(ll); },
      [=](auto h, list<nat> ts) {
        return match(head(const_cast<list<nat>&>(ll)),
          [=](auto &&val) { return val == h; },
          [=]() { return false; });
      });
    });
  */
}

int main() {
  check_list_match();
  return 0;
}
