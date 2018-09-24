# GHC-GRIN

[![Gitter chat](https://badges.gitter.im/grin-tech/grin.png)](https://gitter.im/Grin-Development/Lobby)


GRIN backend for GHC

## Components

### ghc-dump-core ghc-dump-util
custom AST Data type for GHC Core with serialization support and tooling

### ghc-8.2.2
modified GHC which serializes the core AST of each compiled module using `ghc-dump-core`, and calls an external tool to link them at link-time

### lambda-grin
utility (lambda calculus) layer for GRIN frontends

### ghc-grin
converts the dumped GHC Core to Lambda

### ghc-grin-benchmark
sample (stack based) projects to test the modified GHC with the GRIN backend

## Benchmark

Instruction Count Benchmark
- Boquist GRIN on Sparc (RISC) *1999*
- GHC 4.01 on Sparc (RISC) *1999*
- GHC 8.2 on x64 (CISC) *2018*

![Instruction Count Benchmark](boq-grin-ghc-inst-count.png)
