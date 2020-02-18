# GHC-GRIN

[![Gitter chat](https://badges.gitter.im/grin-tech/grin.png)](https://gitter.im/Grin-Development/Lobby)


GRIN backend for GHC

## Status

The project is under heavy development.

Currently we work on GHC primop and FFI support.

GHC-GRIN can not compile programs yet. 

## Components

### external-stg, external-stg-util
custom AST Data type for GHC ~~Core~~ STG with serialization support and tooling

### ghc-8.11.0
modified GHC which serializes the ~~core~~ STG AST of each compiled module using `external-stg`, and calls an external tool to link them at link-time

### lambda-grin
utility (lambda calculus) layer for GRIN frontends

### ghc-grin
converts the dumped GHC ~~Core~~ STG to Lambda

### ghc-grin-benchmark
sample (stack based) projects to test the modified GHC with the GRIN backend

## Setup

Follow these steps to install GHC/GRIN and compile the benchmark programs:

1. Compile included GHC-8.11.0 (*ghc-8.11.0.20200215-src*)  
  `$ ./boot`  
  `$ ./configure`  
  `$ hadrian/build.sh -j --flavour=Quickest`
2. as soon as an error pops up: cannot execute 'grin-ghc'
   create a shell script with just this content (+ chmod 750):
```
#!/bin/sh

exit 0
```
3. Install `llvm-7` (on Mac: `brew install llvm-hs/llvm/llvm-7`)
  this will take some time to finish..
4. Build the executables in directory `ghc-grin/`:  
  `stack setup`  
  `stack build`
5. link the compiled binaries to a directory in your path (e.g. ~/.local/bin)
   and also change the script that the patched GHC will call to:
```
#!/bin/sh

set -e

echo "GRIN Compiler"

ghc-grin $@ | tee ${!#}.out
```
6. Build the benchmark programs in directory `ghc-grin-benchmark`:  
   `./c`  
  - you should see the output of "GRIN Compiler" from the script that calls the grin optimizer


## Preliminary Benchmark

Comparison of Boquist PhD results *(Sparc, RISC)* with GHC 8.2 *(x64, CISC)* based on the CPU instruction count.
This is not an accurate comparison as the CPU architectures differ, instead it gives a rough overview.

Instruction Count Benchmark
- Boquist GRIN on Sparc (RISC) *1999*
- GHC 4.01 on Sparc (RISC) *1999*
- GHC 8.2 on x64 (CISC) *2018*

![Instruction Count Benchmark](boq-grin-ghc-inst-count.png)
