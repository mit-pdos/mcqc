// RUN: %crc %s -o %t.exe
// RUN: %t.exe > FileCheck %s
// CHECK: OK, passed 100 tests
#include <rapidcheck.h>
#include "rclist.hpp"

using namespace rc;

int main() {
  check_lists();
  return 0;
}
