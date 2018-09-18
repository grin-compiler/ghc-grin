{-# LANGUAGE CPP, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif


module Compiler.Hoopl.Unique
  ( Unique, intToUnique
  , UniqueSet, UniqueMap
  , UniqueMonad(..)
  , SimpleUniqueMonad, runSimpleUniqueMonad
  , UniqueMonadT, runUniqueMonadT

  , uniqueToInt -- exposed through GHC module only!
  )

where

import Compiler.Hoopl.Checkpoint
import Compiler.Hoopl.Collections

import qualified Data.IntMap as M
import qualified Data.IntSet as S

import Control.Applicative as AP
import Control.Monad (ap,liftM)
#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
#endif

-----------------------------------------------------------------------------
--		Unique
-----------------------------------------------------------------------------

type Unique = Int

uniqueToInt :: Unique -> Int
uniqueToInt = id

intToUnique :: Int -> Unique
intToUnique = id

-----------------------------------------------------------------------------
-- UniqueSet

newtype UniqueSet = US S.IntSet deriving (Eq, Ord, Show)

instance IsSet UniqueSet where
  type ElemOf UniqueSet = Unique

  setNull (US s) = S.null s
  setSize (US s) = S.size s
  setMember k (US s) = S.member k s

  setEmpty = US S.empty
  setSingleton k = US (S.singleton k)
  setInsert k (US s) = US (S.insert k s)
  setDelete k (US s) = US (S.delete k s)

  setUnion (US x) (US y) = US (S.union x y)
  setDifference (US x) (US y) = US (S.difference x y)
  setIntersection (US x) (US y) = US (S.intersection x y)
  setIsSubsetOf (US x) (US y) = S.isSubsetOf x y

  setFold k z (US s) = S.foldr k z s

  setElems (US s) = S.elems s
  setFromList ks = US (S.fromList ks)

-----------------------------------------------------------------------------
-- UniqueMap

newtype UniqueMap v = UM (M.IntMap v)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance IsMap UniqueMap where
  type KeyOf UniqueMap = Unique

  mapNull (UM m) = M.null m
  mapSize (UM m) = M.size m
  mapMember k (UM m) = M.member k m
  mapLookup k (UM m) = M.lookup k m
  mapFindWithDefault def k (UM m) = M.findWithDefault def k m

  mapEmpty = UM M.empty
  mapSingleton k v = UM (M.singleton k v)
  mapInsert k v (UM m) = UM (M.insert k v m)
  mapInsertWith f k v (UM m) = UM (M.insertWith f k v m)
  mapDelete k (UM m) = UM (M.delete k m)

  mapUnion (UM x) (UM y) = UM (M.union x y)
  mapUnionWithKey f (UM x) (UM y) = UM (M.unionWithKey (f . intToUnique) x y)
  mapDifference (UM x) (UM y) = UM (M.difference x y)
  mapIntersection (UM x) (UM y) = UM (M.intersection x y)
  mapIsSubmapOf (UM x) (UM y) = M.isSubmapOf x y

  mapMap f (UM m) = UM (M.map f m)
  mapMapWithKey f (UM m) = UM (M.mapWithKey (f . intToUnique) m)
  mapFold k z (UM m) = M.foldr k z m
  mapFoldWithKey k z (UM m) = M.foldrWithKey (k . intToUnique) z m
  mapFilter f (UM m) = UM (M.filter f m)

  mapElems (UM m) = M.elems m
  mapKeys (UM m) = M.keys m
  mapToList (UM m) = M.toList m
  mapFromList assocs = UM (M.fromList assocs)
  mapFromListWith f assocs = UM (M.fromListWith f assocs)

----------------------------------------------------------------
-- Monads

class Monad m => UniqueMonad m where
  freshUnique :: m Unique

newtype SimpleUniqueMonad a = SUM { unSUM :: [Unique] -> (a, [Unique]) }

instance Functor SimpleUniqueMonad where
  fmap = liftM

instance Applicative SimpleUniqueMonad where
  pure a = SUM $ \us -> (a, us)
  (<*>) = ap

instance Monad SimpleUniqueMonad where
  return = AP.pure
  m >>= k  = SUM $ \us -> let (a, us') = unSUM m us in
                              unSUM (k a) us'

instance UniqueMonad SimpleUniqueMonad where
  freshUnique = SUM $ f
    where f (u:us) = (u, us)
          f _ = error "Unique.freshUnique(SimpleUniqueMonad): empty list"

instance CheckpointMonad SimpleUniqueMonad where
  type Checkpoint SimpleUniqueMonad = [Unique]
  checkpoint = SUM $ \us -> (us, us)
  restart us = SUM $ \_  -> ((), us)

runSimpleUniqueMonad :: SimpleUniqueMonad a -> a
runSimpleUniqueMonad m = fst (unSUM m allUniques)

----------------------------------------------------------------

newtype UniqueMonadT m a = UMT { unUMT :: [Unique] -> m (a, [Unique]) }

instance Monad m => Functor (UniqueMonadT m) where
  fmap  = liftM

instance Monad m => Applicative (UniqueMonadT m) where
  pure a = UMT $ \us -> return (a, us)
  (<*>) = ap

instance Monad m => Monad (UniqueMonadT m) where
  return = pure
  m >>= k  = UMT $ \us -> do { (a, us') <- unUMT m us; unUMT (k a) us' }

instance Monad m => UniqueMonad (UniqueMonadT m) where
  freshUnique = UMT $ f
    where f (u:us) = return (u, us)
          f _ = error "Unique.freshUnique(UniqueMonadT): empty list"

runUniqueMonadT :: Monad m => UniqueMonadT m a -> m a
runUniqueMonadT m = do { (a, _) <- unUMT m allUniques; return a }

allUniques :: [Unique]
allUniques = [1..]
