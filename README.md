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

We're using `lit` for testing, use:
```
lit test
```

# Memory Profiling

Go to either of the above and run
```
make prof
```

# Maintainer

Lef Ioannidis <elefthei@mit.edu>
