{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 700
{-# LANGUAGE DeriveDataTypeable #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Identity
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The identity functor and monad.
--
-- This trivial type constructor serves two purposes:
--
-- * It can be used with functions parameterized by functor or monad classes.
--
-- * It can be used as a base monad to which a series of monad
--   transformers may be applied to construct a composite monad.
--   Most monad transformer modules include the special case of
--   applying the transformer to 'Identity'.  For example, @State s@
--   is an abbreviation for @StateT s 'Identity'@.
-----------------------------------------------------------------------------

module Data.Functor.Identity (
    Identity(..),
  ) where

import Control.Applicative
import Control.Monad.Fix
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith, munzip))
#endif
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (Monoid(mempty, mappend))
import Data.Traversable (Traversable(traverse))
#if __GLASGOW_HASKELL__ >= 700
import Data.Data
#endif
import Data.Ix (Ix(..))
import Foreign (Storable(..), castPtr)
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

-- | Identity functor and monad. (a non-strict monad)
newtype Identity a = Identity { runIdentity :: a }
    deriving ( Eq, Ord
#if __GLASGOW_HASKELL__ >= 700
             , Data, Typeable
#endif
#if __GLASGOW_HASKELL__ >= 702
             , Generic
#endif
#if __GLASGOW_HASKELL__ >= 706
             , Generic1
#endif
             )

instance (Bounded a) => Bounded (Identity a) where
    minBound = Identity minBound
    maxBound = Identity maxBound

instance (Enum a) => Enum (Identity a) where
    succ (Identity x)     = Identity (succ x)
    pred (Identity x)     = Identity (pred x)
    toEnum i              = Identity (toEnum i)
    fromEnum (Identity x) = fromEnum x
    enumFrom (Identity x) = map Identity (enumFrom x)
    enumFromThen (Identity x) (Identity y) = map Identity (enumFromThen x y)
    enumFromTo   (Identity x) (Identity y) = map Identity (enumFromTo   x y)
    enumFromThenTo (Identity x) (Identity y) (Identity z) =
        map Identity (enumFromThenTo x y z)

instance (Ix a) => Ix (Identity a) where
    range     (Identity x, Identity y) = map Identity (range (x, y))
    index     (Identity x, Identity y) (Identity i) = index     (x, y) i
    inRange   (Identity x, Identity y) (Identity e) = inRange   (x, y) e
    rangeSize (Identity x, Identity y) = rangeSize (x, y)

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity x) (Identity y) = Identity (mappend x y)

-- These instances would be equivalent to the derived instances of the
-- newtype if the field were removed.

instance (Read a) => Read (Identity a) where
    readsPrec d = readParen (d > 10) $ \ r ->
        [(Identity x,t) | ("Identity",s) <- lex r, (x,t) <- readsPrec 11 s]

instance (Show a) => Show (Identity a) where
    showsPrec d (Identity x) = showParen (d > 10) $
        showString "Identity " . showsPrec 11 x

instance (Storable a) => Storable (Identity a) where
    sizeOf    (Identity x)       = sizeOf x
    alignment (Identity x)       = alignment x
    peekElemOff p i              = fmap Identity (peekElemOff (castPtr p) i)
    pokeElemOff p i (Identity x) = pokeElemOff (castPtr p) i x
    peekByteOff p i              = fmap Identity (peekByteOff p i)
    pokeByteOff p i (Identity x) = pokeByteOff p i x
    peek p                       = fmap runIdentity (peek (castPtr p))
    poke p (Identity x)          = poke (castPtr p) x

-- ---------------------------------------------------------------------------
-- Identity instances for Functor and Monad

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))

instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x

instance Applicative Identity where
    pure a = Identity a
    Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)

instance MonadFix Identity where
    mfix f = Identity (fix (runIdentity . f))

#if MIN_VERSION_base(4,4,0)
instance MonadZip Identity where
    mzipWith f (Identity x) (Identity y) = Identity (f x y)
    munzip (Identity (a, b)) = (Identity a, Identity b)
#endif
