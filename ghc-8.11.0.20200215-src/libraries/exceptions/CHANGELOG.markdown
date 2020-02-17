next [????.??.??]
-----------------
* Only depend on `transformers-compat` on old versions of GHC.

0.10.3 [2019.08.27]
-------------------
* `MonadThrow` instance for the strict `ST` monad.

0.10.2 [2019.05.02]
-------------------
* Allow building with `base-4.13`/`template-haskell-2.15`.

0.10.1 [2019.03.26]
-------------------
* Define a `MonadFail` instance for `CatchT`.
* Allow `QuickCheck-2.13` in the test suite.

0.10.0
------
* Fix a regression in 0.9.0 whereby the non-IO effects in `bracket`'s `use`
  action were not visible to the `release` action, and the non-IO effects in the
  `release` action were not visible after the `bracket` call.
* The type of `generalBracket` was changed in order to restore those non-IO
  effects, so if you are a library author that provides a `MonadMask` instance,
  you will need to update your implementation of this method.
* Add `MonadMask` instance for `MaybeT`
* Add `onError` function whose action also runs on errors which are not
  exceptions, such as a `Nothing` or a `Left`.

0.9.0
-----
* Add `generalBracket` to the `MonadMask` typeclass, allowing more
  valid instances.

  Note that functions such as `bracket` and `finally` are now based off of
  `generalBracket`, so if you are a library author that provides a `MonadMask`
  instance, you will need to provide an implementation of this method.
* Add `MonadMask` instances for `ExceptT` and `ErrorT`

0.8.3
-----
* `MonadCatch` and `MonadMask` instances for `Either SomeException`

0.8.1
-----
* Support for throwing in the `template-haskell` `Q` monad
* Support for `transformers` 0.5

0.8.0.1
-------
* Resolved warnings on GHC 7.10 and with transformers 0.4.

0.8
---
* Use `transformers-compat` to allow support for `ExceptT` even on older `transformers` versions.

0.7
---
* `stm` support

0.6
---
* Split out `MonadMask`
* Added `transformers` 0.4 support

0.5
---
* Added instances of `MonadThrow` for `ListT`, `MaybeT`, `ErrorT` and `ContT`.

0.4
---
* Factored out a separate `MonadThrow`.

0.3.3.1
-------
* QuickCheck dependency bump

0.3.3
-----
* Relicensed under the 3-clause BSD license.

0.3.2
-----
* Better documentation for `CatchT`.
* Added `handle`-like analogues for parity with `Control.Exception`.

0.3.1
-----
* Fixed test suite.

0.3
---
* Moved `CatchT` to `Control.Monad.Catch.Pure` to make it clear it isn't required for working with `IO`.

0.2.1
---
* Added `mask_` and `uninterruptibleMask_` to `Control.Monad.Catch`.

0.2
---
* Added `uninterruptibleMask` to `MonadCatch`.

0.1.1
-----
* Flagged `Control.Monad.Catch` as `Trustworthy`

0.1.0.1
-----
* License fix. We were accidentally listing both an APL and BSD3 license in the same module

0.1
---
* Repository initialized
