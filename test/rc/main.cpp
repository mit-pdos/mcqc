// RUN: %crc %s -o %t.exe
// RUN: %t.exe > FileCheck %s
// CHECK: OK, passed 100 tests
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

#include <proc.hpp>
#include <show.hpp>
using namespace Proc;
using namespace Show;

#include <iostream>
int main() {
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


  // Match and head cancel out
  rc::check("List match & head", [](const list<string>& l) {
    list<string>& ll = const_cast<list<string>&>(l);
    return match(ll,
      [=](){ return ll.empty(); },
      [=](auto &&h, list<string>&& ts) {
        print(show(ll));
        std::cout << "Tail: ";
        print(show(ts));
        bool ret = match(copy(head(const_cast<list<string>&>(ll))),
            [=](auto&& some){
                std::cout << h << " ===== VS ===== " << some << std::endl;
                std::cout << "Comparison " << (h == some ? "true" : "false") << std::endl;
                return h == some; },
            []() { return false; });
        std::cout << "Whole match returned " << (ret ? "true" : "false") << std::endl;
        return ret;
      });
  });


  return 0;
}
