# Makefile

all: main; ./main

main: main.cpp Hello.o; g++ \
  -liconv \
  -I/usr/lib/ghc/include \
  -L/usr/lib/ghc/ghc-8.6.5 \
  -lHSrts \
  -L/usr/local/Cellar/ghc/7.6.3/lib/ghc-7.6.3/base-4.6.0.1 \
  -lHSbase-4.6.0.1 \
  -L/usr/local/Cellar/ghc/7.6.3/lib/ghc-7.6.3/ghc-prim-0.3.0.0 \
  -lHSghc-prim-0.3.0.0 \
  -L/usr/local/Cellar/ghc/7.6.3/lib/ghc-7.6.3/integer-gmp-0.5.0.0 \
  -lHSinteger-gmp-0.5.0.0 \
  -lHSghc-prim-0.3.0.0 \
  -fno-stack-protector \
  -Wall \
  -o main main.cpp Hello.o

Hello.o: Hello.hs; ghc -fforce-recomp Hello.hs

.PHONY: clean
clean: ; rm -rf main && rm -rf *.o && rm -rf *.hi && rm -rf *_stub.h