#include <rapidcheck.h>

// Ours
#include <list.hpp>
#include <string.hpp>
#include <copy.hpp>
#include <type_checks.h>

using namespace rc;

// Ours
using namespace List;
using namespace String;
using namespace Copy;

void check_lists() {
  // Match and cons cancel out
  rc::check("List match & cons", [](const list<string>& l) {
    list<string>& ll = const_cast<list<string>&>(l);
    return match(copy(ll),
      [=](){ return empty(ll); },
      [=](auto &&h, list<string>&& ts) { return cons(h, ts) == ll; });
  });

  // Match and tail cancel out
  rc::check("List match & tail", [](const list<string>& l) {
    list<string> ll = const_cast<list<string>&>(l);
    return match(copy(ll),
      [=](){ return empty(ll); },
      [=](auto &&h, list<string>&& ts) { return ts == tail(const_cast<list<string>&>(ll)); });
  });

  // Match and app cancel out
  rc::check("List match & app", [](const list<string>& l) {
    list<string> ll = const_cast<list<string>&>(l);
    return match(copy(ll),
      [=](){ return empty(ll); },
      [=](auto &&h, list<string>&& ts) { return app(list<string>{ h }, ts) == ll; });
  });

  // Match and app cancel out
  rc::check("List match & app with forwarding", [](const list<string>& l) {
    list<string> ll = const_cast<list<string>&>(l);
    return match(copy(ll),
      [=](){ return empty(ll); },
      [=](auto &&h, list<string>&& ts) { return app(list<string>{ h }, FWD(ts)) == FWD(const_cast<list<string>&>(ll)); });
  });
}
