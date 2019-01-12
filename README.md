[![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/mit-pdos/machcoq)

<img src="https://elefthei.github.io/assets/css/images/machcoq.jpg" alt="MCQC" width="200"/>

MCQC Coq to C++17 compiler
--------------------------

# Introduction

Alternative Coq extractor to C++ written in Haskell.
Takes in JSON extraction (available after Coq v.8.5.1) and exports valid, performant and memory safe C++17.
See `test/numeric` and `test/cat` for examples. No GC no RTS, uses `shared_ptr` for reference
counting and a library of base types (in `include/*.hpp`).

# Building

Then with stack, run the following in the source root:
```
stack build
```

# Testing

First, cd into `classes` and run `make`.
This will generate the object files for Coq typeclasses used.

Uses `llvm-lit` for testing, installable from `pip`.
```
lit test
```

To enable property based testing with RapidCheck of the C++ library, run:
```
RC=on lit test/rc
```

# Maintainer
Lef Ioannidis <elefthei@mit.edu>
