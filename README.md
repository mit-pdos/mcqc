Coq to functional C++17 extractor
--------------------------

# Introduction

Alternative extractor to C++, no GC no RTS, pure static.

# Building

In the source root:
```
cabal install
cabal build
```

Or use cabal sandbox if you prefer.

# Testing

Go in `test/fib` and type:

```
make
./fib
```

Go in `test/rev` and type:
```
make
./rev
```

# Memory Profiling

Go to either of the above and run
```
make prof
```

# Maintainer

Lef Ioannidis <elefthei@mit.edu>
