The `hoopl` Package  [![Hackage](https://img.shields.io/hackage/v/hoopl.svg)](https://hackage.haskell.org/package/hoopl) [![Build Status](https://travis-ci.org/haskell/hoopl.svg)](https://travis-ci.org/haskell/hoopl)
===================

## Hoopl: A Higher-Order OPtimization Library

API documentation can be found on
[Hackage](https://hackage.haskell.org/package/hoopl).  For detailed explanation
of the library design see paper ["Hoopl: A Modular, Reusable Library for
Dataflow Analysis and
Transformation"](http://research.microsoft.com/en-us/um/people/simonpj/Papers/c--/hoopl-haskell10.pdf)

| Directory      | Contents
| -------------- | ---------
| `src/`         | The current official sources to the Cabal package
| `testing/`     | Tests, including a sample client.  See [`testing/README`](testing/README)

### Development Notes

#### Building and testing

To build the library run:

    cabal configure
    cabal build
    cabal install --enable-documentation

To run the tests in the `testing/` folder run:

    cabal configure --enable-tests
    cabal test

To run the tests with the test coverage report run:

    cabal configure --enable-tests --enable-coverage
    cabal test

You'll need a Haskell Platform, which should include appropriate
versions of Cabal and GHC.

#### Coding style

Please follow Johan Tibell's
[Haskell Style Guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)
for all new/modified code.

### Checklist for Making Releases

In order to facilitate GHC development's workflow, the version in [`hoopl.cabal`](hoopl.cabal) is to be bumped as soon as a change requires a respective version bump (according to the PVP) relative to the last released `hoopl` version.

1. Make sure `hoopl` passes Travis for all GHC versions in the build-matrix
2. Update Changelog (& `git commit`)
3. Generate source tarball via `cabal sdist` and upload a candidate to Hackage (see note below), and inspect the result. 
4. If everything checks out, make an annotated and GPG-signed Git release tag: `git tag -a -s v${VER} -m "hoopl ${VER}"`
5. Publish (there's a button for that on Hackage) the package candidate
6. Work on next release

Note: To upload to Hackage,

    cabal sdist
    cabal upload dist/hoopl-*.tar.gz

However, it's recommended use the Hackage feature for
[uploading a candidate](http://hackage.haskell.org/packages/candidates/upload).
