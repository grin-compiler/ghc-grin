{-# LANGUAGE CPP, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------
--		The fuel monad
-----------------------------------------------------------------------------

module Compiler.Hoopl.Fuel
  ( Fuel, infiniteFuel, fuelRemaining
  , withFuel
  , FuelMonad(..)
  , FuelMonadT(..)
  , CheckingFuelMonad
  , InfiniteFuelMonad
  , SimpleFuelMonad
  )
where

import Compiler.Hoopl.Checkpoint
import Compiler.Hoopl.Unique

import Control.Applicative as AP (Applicative(..))
import Control.Monad (ap,liftM)

class Monad m => FuelMonad m where
  getFuel :: m Fuel
  setFuel :: Fuel -> m ()

-- | Find out how much fuel remains after a computation.
-- Can be subtracted from initial fuel to get total consumption.
fuelRemaining :: FuelMonad m => m Fuel
fuelRemaining = getFuel

class FuelMonadT fm where
  runWithFuel :: (Monad m, FuelMonad (fm m)) => Fuel -> fm m a -> m a
  liftFuel    :: (Monad m, FuelMonad (fm m)) => m a -> fm m a



type Fuel = Int

withFuel :: FuelMonad m => Maybe a -> m (Maybe a)
withFuel Nothing  = return Nothing
withFuel (Just a) = do f <- getFuel
                       if f == 0
                         then return Nothing
                         else setFuel (f-1) >> return (Just a)


----------------------------------------------------------------

newtype CheckingFuelMonad m a = FM { unFM :: Fuel -> m (a, Fuel) }

instance Monad m => Functor (CheckingFuelMonad m) where
  fmap  = liftM

instance Monad m => Applicative (CheckingFuelMonad m) where
  pure a = FM (\f -> return (a, f))
  (<*>) = ap

instance Monad m => Monad (CheckingFuelMonad m) where
  return = AP.pure
  fm >>= k = FM (\f -> do { (a, f') <- unFM fm f; unFM (k a) f' })

instance CheckpointMonad m => CheckpointMonad (CheckingFuelMonad m) where
  type Checkpoint (CheckingFuelMonad m) = (Fuel, Checkpoint m)
  checkpoint = FM $ \fuel -> do { s <- checkpoint
                                ; return ((fuel, s), fuel) }
  restart (fuel, s) = FM $ \_ -> do { restart s; return ((), fuel) }

instance UniqueMonad m => UniqueMonad (CheckingFuelMonad m) where
  freshUnique = FM (\f -> do { l <- freshUnique; return (l, f) })

instance Monad m => FuelMonad (CheckingFuelMonad m) where
  getFuel   = FM (\f -> return (f, f))
  setFuel f = FM (\_ -> return ((),f))

instance FuelMonadT CheckingFuelMonad where
  runWithFuel fuel m = do { (a, _) <- unFM m fuel; return a }
  liftFuel m = FM $ \f -> do { a <- m; return (a, f) }

----------------------------------------------------------------

newtype InfiniteFuelMonad m a = IFM { unIFM :: m a }

instance Monad m => Functor (InfiniteFuelMonad m) where
  fmap  = liftM

instance Monad m => Applicative (InfiniteFuelMonad m) where
  pure a = IFM $ return a
  (<*>) = ap

instance Monad m => Monad (InfiniteFuelMonad m) where
  return = pure
  m >>= k  = IFM $ do { a <- unIFM m; unIFM (k a) }

instance UniqueMonad m => UniqueMonad (InfiniteFuelMonad m) where
  freshUnique = IFM $ freshUnique

instance Monad m => FuelMonad (InfiniteFuelMonad m) where
  getFuel   = return infiniteFuel
  setFuel _ = return ()

instance CheckpointMonad m => CheckpointMonad (InfiniteFuelMonad m) where
  type Checkpoint (InfiniteFuelMonad m) = Checkpoint m
  checkpoint = IFM checkpoint
  restart s  = IFM $ restart s



instance FuelMonadT InfiniteFuelMonad where
  runWithFuel _ = unIFM
  liftFuel = IFM

infiniteFuel :: Fuel -- effectively infinite, any, but subtractable
infiniteFuel = maxBound

type SimpleFuelMonad = CheckingFuelMonad SimpleUniqueMonad

{-
runWithFuelAndUniques :: Fuel -> [Unique] -> FuelMonad a -> a
runWithFuelAndUniques fuel uniques m = a
  where (a, _, _) = unFM m fuel uniques

freshUnique :: FuelMonad Unique
freshUnique = FM (\f (l:ls) -> (l, f, ls))
-}

