# External STM testsuite

This testsuite is designed to work with various `stm` versions in
order to facilitate implementing and verifying regression testing.

## Testing in-tree `stm`

For testing the in-tree `stm` version in the parent folder `../`, invoke

    cabal new-run test:stm -w ghc-7.10.3

from the top-level folder of the `stm` Git repo.

## Testing relased `stm` versions

For testing released versions of `stm`, run testsuite from this folder
where the `testsuite` package resides like so

    cabal new-run test:stm -w ghc-7.10.3 --constraint 'stm == 2.4.5.0'

this allows you to conveniently control the GHC as well as the version
of 'stm' under test.
