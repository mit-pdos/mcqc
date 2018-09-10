[![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/mit-pdos/machcoq)

<img src="https://elefthei.github.io/assets/css/images/machcoq.jpg" alt="MachCoq" width="200"/>

Coq to functional C++17 extractor
--------------------------

# Introduction

Alternative extractor to C++, no GC no RTS, pure static.

# Building
With stack , run the following in the source root:
```
stack build
```

# Testing
Uses `llvm-lit` for testing, installable from `pip`.
```
lit test
```

# Maintainer
Lef Ioannidis <elefthei@mit.edu>
