Coq to functional C++17 extractor
--------------------------

# Introduction

Alternative extractor to C++, no GC no RTS, pure static.

# Building

With stack in the source root:
```
stack build
```

Or use cabal sandbox if you prefer.

# Testing

We're using `llvm-lit` for testing, use:
```
lit test
```


Lef Ioannidis <elefthei@mit.edu>
