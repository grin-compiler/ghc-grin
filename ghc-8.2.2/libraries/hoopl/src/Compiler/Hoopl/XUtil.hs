{-# LANGUAGE CPP, GADTs, RankNTypes, ScopedTypeVariables, TypeFamilies  #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-- | Utilities for clients of Hoopl, not used internally.

module Compiler.Hoopl.XUtil
  (
    -- * Utilities for clients
    firstXfer, distributeXfer
  , distributeFact, distributeFactBwd
  , successorFacts
  , joinFacts
  , joinOutFacts -- deprecated
  , joinMaps
  , analyzeAndRewriteFwdBody, analyzeAndRewriteBwdBody
  , analyzeAndRewriteFwdOx, analyzeAndRewriteBwdOx
  )
where

import qualified Data.Map as M
import Data.Maybe

import Compiler.Hoopl.Collections
import Compiler.Hoopl.Checkpoint
import Compiler.Hoopl.Dataflow
import Compiler.Hoopl.Block
import Compiler.Hoopl.Graph
import Compiler.Hoopl.Label

-----------------------------------------------------------------------------

-- | Forward dataflow analysis and rewriting for the special case of a Body.
-- A set of entry points must be supplied; blocks not reachable from
-- the set are thrown away.
analyzeAndRewriteFwdBody
   :: forall m n f entries. (CheckpointMonad m, NonLocal n, LabelsPtr entries)
   => FwdPass m n f
   -> entries -> Body n -> FactBase f
   -> m (Body n, FactBase f)

-- | Backward dataflow analysis and rewriting for the special case of a Body.
-- A set of entry points must be supplied; blocks not reachable from
-- the set are thrown away.
analyzeAndRewriteBwdBody
   :: forall m n f entries. (CheckpointMonad m, NonLocal n, LabelsPtr entries)
   => BwdPass m n f 
   -> entries -> Body n -> FactBase f 
   -> m (Body n, FactBase f)

analyzeAndRewriteFwdBody pass en = mapBodyFacts (analyzeAndRewriteFwd pass (JustC en))
analyzeAndRewriteBwdBody pass en = mapBodyFacts (analyzeAndRewriteBwd pass (JustC en))

mapBodyFacts :: (Monad m)
    => (Graph n C C -> Fact C f   -> m (Graph n C C, Fact C f, MaybeO C f))
    -> (Body n      -> FactBase f -> m (Body n, FactBase f))
-- ^ Internal utility; should not escape
mapBodyFacts anal b f = anal (GMany NothingO b NothingO) f >>= bodyFacts
  where -- the type constraint is needed for the pattern match;
        -- if it were not, we would use do-notation here.
    bodyFacts :: Monad m => (Graph n C C, Fact C f, MaybeO C f) -> m (Body n, Fact C f)
    bodyFacts (GMany NothingO body NothingO, fb, NothingO) = return (body, fb)

{-
  Can't write:

     do (GMany NothingO body NothingO, fb, NothingO) <- anal (....) f
        return (body, fb)

  because we need an explicit type signature in order to do the GADT
  pattern matches on NothingO
-}



-- | Forward dataflow analysis and rewriting for the special case of a 
-- graph open at the entry.  This special case relieves the client
-- from having to specify a type signature for 'NothingO', which beginners
-- might find confusing and experts might find annoying.
analyzeAndRewriteFwdOx
   :: forall m n f x. (CheckpointMonad m, NonLocal n)
   => FwdPass m n f -> Graph n O x -> f -> m (Graph n O x, FactBase f, MaybeO x f)

-- | Backward dataflow analysis and rewriting for the special case of a 
-- graph open at the entry.  This special case relieves the client
-- from having to specify a type signature for 'NothingO', which beginners
-- might find confusing and experts might find annoying.
analyzeAndRewriteBwdOx
   :: forall m n f x. (CheckpointMonad m, NonLocal n)
   => BwdPass m n f -> Graph n O x -> Fact x f -> m (Graph n O x, FactBase f, f)

-- | A value that can be used for the entry point of a graph open at the entry.
noEntries :: MaybeC O Label
noEntries = NothingC

analyzeAndRewriteFwdOx pass g f  = analyzeAndRewriteFwd pass noEntries g f
analyzeAndRewriteBwdOx pass g fb = analyzeAndRewriteBwd pass noEntries g fb >>= strip
  where strip :: forall m a b c . Monad m => (a, b, MaybeO O c) -> m (a, b, c)
        strip (a, b, JustO c) = return (a, b, c)





-- | A utility function so that a transfer function for a first
-- node can be given just a fact; we handle the lookup.  This
-- function is planned to be made obsolete by changes in the dataflow
-- interface.

firstXfer :: NonLocal n => (n C O -> f -> f) -> (n C O -> FactBase f -> f)
firstXfer xfer n fb = xfer n $ fromJust $ lookupFact (entryLabel n) fb

-- | This utility function handles a common case in which a transfer function
-- produces a single fact out of a last node, which is then distributed
-- over the outgoing edges.
distributeXfer :: NonLocal n
               => DataflowLattice f -> (n O C -> f -> f) -> (n O C -> f -> FactBase f)
distributeXfer lattice xfer n f =
    mkFactBase lattice [ (l, xfer n f) | l <- successors n ]


-- | This utility function handles a common case in which a transfer function
-- for a last node takes the incoming fact unchanged and simply distributes
-- that fact over the outgoing edges.
distributeFact :: NonLocal n => n O C -> f -> FactBase f
distributeFact n f = mapFromList [ (l, f) | l <- successors n ]
   -- because the same fact goes out on every edge,
   -- there's no need for 'mkFactBase' here.

-- | This utility function handles a common case in which a backward transfer
-- function takes the incoming fact unchanged and tags it with the node's label.
distributeFactBwd :: NonLocal n => n C O -> f -> FactBase f
distributeFactBwd n f = mapSingleton (entryLabel n) f

-- | List of (unlabelled) facts from the successors of a last node
successorFacts :: NonLocal n => n O C -> FactBase f -> [f]
successorFacts n fb = [ f | id <- successors n, let Just f = lookupFact id fb ]

-- | Join a list of facts.
joinFacts :: DataflowLattice f -> Label -> [f] -> f
joinFacts lat inBlock = foldr extend (fact_bot lat)
  where extend new old = snd $ fact_join lat inBlock (OldFact old) (NewFact new)

{-# DEPRECATED joinOutFacts
    "should be replaced by 'joinFacts lat l (successorFacts n f)'; as is, it uses the wrong Label" #-}

joinOutFacts :: (NonLocal node) => DataflowLattice f -> node O C -> FactBase f -> f
joinOutFacts lat n f = foldr join (fact_bot lat) facts
  where join (lbl, new) old = snd $ fact_join lat lbl (OldFact old) (NewFact new)
        facts = [(s, fromJust fact) | s <- successors n, let fact = lookupFact s f, isJust fact]


-- | It's common to represent dataflow facts as a map from variables
-- to some fact about the locations. For these maps, the join
-- operation on the map can be expressed in terms of the join on each
-- element of the codomain:
joinMaps :: Ord k => JoinFun v -> JoinFun (M.Map k v)
joinMaps eltJoin l (OldFact old) (NewFact new) = M.foldrWithKey add (NoChange, old) new
  where 
    add k new_v (ch, joinmap) =
      case M.lookup k joinmap of
        Nothing    -> (SomeChange, M.insert k new_v joinmap)
        Just old_v -> case eltJoin l (OldFact old_v) (NewFact new_v) of
                        (SomeChange, v') -> (SomeChange, M.insert k v' joinmap)
                        (NoChange,   _)  -> (ch, joinmap)



