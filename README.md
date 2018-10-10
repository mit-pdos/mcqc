[![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/mit-pdos/machcoq)

<img src="https://elefthei.github.io/assets/css/images/machcoq.jpg" alt="MachCoq" width="200"/>

Coq to functional C++17 extractor
--------------------------

# Introduction

Alternative extractor to C++, no GC no RTS, pure static.

# Building

First, cd into `classes` and run `make`.
This will generate the object files for Coq typeclasses used.

Then with stack , run the following in the source root:
```
stack build
```

# Testing
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
