{-# LANGUAGE CPP, GADTs, ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-- | Possibly doubly pointed lattices

module Compiler.Hoopl.Pointed
  ( Pointed(..), addPoints, addPoints', addTop, addTop'
  , liftJoinTop, extendJoinDomain
  , WithTop, WithBot, WithTopAndBot
  )
where

import Compiler.Hoopl.Block
import Compiler.Hoopl.Label
import Compiler.Hoopl.Dataflow

-- | Adds top, bottom, or both to help form a lattice
data Pointed t b a where
   Bot   ::      Pointed t C a
   PElem :: a -> Pointed t b a
   Top   ::      Pointed C b a

-- ^ The type parameters 't' and 'b' are used to say whether top
-- and bottom elements have been added.  The analogy with 'Block'
-- is nearly exact:
--
--  * A 'Block' is closed at the entry if and only if it has a first node;
--    a 'Pointed' is closed at the top if and only if it has a top element.
--
--  * A 'Block' is closed at the exit if and only if it has a last node;
--    a 'Pointed' is closed at the bottom if and only if it has a bottom element.
--
-- We thus have four possible types, of which three are interesting:
--
--  [@Pointed C C a@] Type @a@ extended with both top and bottom elements.
--
--  [@Pointed C O a@] Type @a@ extended with a top element
--  only. (Presumably @a@ comes equipped with a bottom element of its own.)
--
--  [@Pointed O C a@] Type @a@ extended with a bottom element only. 
--
--  [@Pointed O O a@] Isomorphic to @a@, and therefore not interesting.
--
-- The advantage of all this GADT-ishness is that the constructors
-- 'Bot', 'Top', and 'PElem' can all be used polymorphically.
--
-- A 'Pointed t b' type is an instance of 'Functor' and 'Show'.



type WithBot a = Pointed O C a
-- ^ Type 'a' with a bottom element adjoined

type WithTop a = Pointed C O a
-- ^ Type 'a' with a top element adjoined

type WithTopAndBot a = Pointed C C a
-- ^ Type 'a' with top and bottom elements adjoined


-- | Given a join function and a name, creates a semi lattice by
-- adding a bottom element, and possibly a top element also.
-- A specialized version of 'addPoints''.
addPoints  :: String -> JoinFun a -> DataflowLattice (Pointed t C a)
-- | A more general case for creating a new lattice
addPoints' :: forall a t .
              String
           -> (Label -> OldFact a -> NewFact a -> (ChangeFlag, Pointed t C a))
           -> DataflowLattice (Pointed t C a)

addPoints name join = addPoints' name join'
   where join' l o n = (change, PElem f)
            where (change, f) = join l o n

addPoints' name joinx = DataflowLattice name Bot join
  where -- careful: order of cases matters for ChangeFlag
        join :: JoinFun (Pointed t C a)
        join _ (OldFact f)            (NewFact Bot) = (NoChange, f)
        join _ (OldFact Top)          (NewFact _)   = (NoChange, Top)
        join _ (OldFact Bot)          (NewFact f)   = (SomeChange, f)
        join _ (OldFact _)            (NewFact Top) = (SomeChange, Top)
        join l (OldFact (PElem old)) (NewFact (PElem new))
           = joinx l (OldFact old) (NewFact new)


liftJoinTop :: JoinFun a -> JoinFun (WithTop a)
extendJoinDomain :: forall a
              . (Label -> OldFact a -> NewFact a -> (ChangeFlag, WithTop a))
             -> JoinFun (WithTop a)

extendJoinDomain joinx = join
 where join :: JoinFun (WithTop a)
       join _ (OldFact Top)          (NewFact _)   = (NoChange, Top)
       join _ (OldFact _)            (NewFact Top) = (SomeChange, Top)
       join l (OldFact (PElem old)) (NewFact (PElem new))
           = joinx l (OldFact old) (NewFact new)

liftJoinTop joinx = extendJoinDomain (\l old new -> liftPair $ joinx l old new)
  where liftPair (c, a) = (c, PElem a)

-- | Given a join function and a name, creates a semi lattice by
-- adding a top element but no bottom element.  Caller must supply the bottom 
-- element.
addTop  :: DataflowLattice a -> DataflowLattice (WithTop a)
-- | A more general case for creating a new lattice
addTop' :: forall a .
              String
           -> a
           -> (Label -> OldFact a -> NewFact a -> (ChangeFlag, WithTop a))
           -> DataflowLattice (WithTop a)

addTop lattice = addTop' name' (fact_bot lattice) join'
   where name' = fact_name lattice ++ " + T"
         join' l o n = (change, PElem f)
            where (change, f) = fact_join lattice l o n

addTop' name bot joinx = DataflowLattice name (PElem bot) join
  where -- careful: order of cases matters for ChangeFlag
        join :: JoinFun (WithTop a)
        join _ (OldFact Top)          (NewFact _)   = (NoChange, Top)
        join _ (OldFact _)            (NewFact Top) = (SomeChange, Top)
        join l (OldFact (PElem old)) (NewFact (PElem new))
           = joinx l (OldFact old) (NewFact new)

instance Show a => Show (Pointed t b a) where
  show Bot = "_|_"
  show Top = "T"
  show (PElem a) = show a

instance Functor (Pointed t b) where
  fmap _ Bot = Bot
  fmap _ Top = Top
  fmap f (PElem a) = PElem (f a)

instance Eq a => Eq (Pointed t b a) where
  Bot == Bot = True
  Top == Top = True
  (PElem a) == (PElem a') = a == a'
  _ == _ = False

instance Ord a => Ord (Pointed t b a) where
  Bot     `compare` Bot      = EQ
  Bot     `compare` _        = LT
  _       `compare` Bot      = GT
  PElem a `compare` PElem a' = a `compare` a'
  Top     `compare` Top      = EQ
  Top     `compare` _        = GT
  _       `compare` Top      = LT
