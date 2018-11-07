## 0.68.5

 - Support response files regardless of which GHC `hsc2hs` was compiled
   with ([#15758](https://ghc.haskell.org/trac/ghc/ticket/15758))

 - Support for non-x86 platforms should be significantly more robust due to
   improvements in `hsc2hs`'s assembly parser

## 0.68.4

 - Add support to read command line arguments supplied via response files
   ([#13896](https://ghc.haskell.org/trac/ghc/ticket/13388))

## 0.68.2

 - Support GHC 8.2.1

 - Make `hsc_alignment` macro work in clang
   ([D3346](https://phabricator.haskell.org/D3346))

 - Track column numbers to improve GHC's caret diagnostic display
   ([#13388](https://ghc.haskell.org/trac/ghc/ticket/13388))

## 0.68.1

 - Fix type signature of generated `main` test function
   to avoid C compiler warnings about unused `argc`/`argv`
   function parameters during feature testing.

 - Double-escape paths used to build call to `hsc_line`
   ([#12504](http://ghc.haskell.org/ticket/12504))
