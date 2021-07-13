#include <iostream>
#include "hello_stub.h"
#include "/usr/lib/ghc/include/HsFFI.h"

int main(int argc, char** argv) {
  hs_init(&argc, &argv);
  std::cout << "Hello from C++\n";
  helloFromHaskell();
  hs_exit();
  return 0;
}