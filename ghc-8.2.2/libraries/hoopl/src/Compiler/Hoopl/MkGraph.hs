{-# LANGUAGE CPP, ScopedTypeVariables, GADTs, TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.MkGraph
    ( AGraph, graphOfAGraph, aGraphOfGraph
    , (<*>), (|*><*|), catGraphs, addEntrySeq, addExitSeq, addBlocks, unionBlocks
    , emptyGraph, emptyClosedGraph, withFresh
    , mkFirst, mkMiddle, mkMiddles, mkLast, mkBranch, mkLabel, mkWhileDo
    , IfThenElseable(mkIfThenElse)
    , mkEntry, mkExit
    , HooplNode(mkLabelNode, mkBranchNode)
    )
where

import Compiler.Hoopl.Label (Label, uniqueToLbl)
import Compiler.Hoopl.Block
import Compiler.Hoopl.Graph as U
import Compiler.Hoopl.Unique

import Control.Monad (Monad(..),liftM2)
import Prelude (($),(.),foldr,map) -- for the purpose of 'hiding ((<*>))'

{-|
As noted in the paper, we can define a single, polymorphic type of 
splicing operation with the very polymorphic type
@
  AGraph n e a -> AGraph n a x -> AGraph n e x
@
However, we feel that this operation is a bit /too/ polymorphic,
and that it's too easy for clients to use it blindly without 
thinking.  We therfore split it into two operations, '<*>' and '|*><*|', 
which are supplemented by other functions:

  * The '<*>' operator is true concatenation, for connecting open graphs.
    Control flows from the left graph to the right graph.

  * The '|*><*|' operator splices together two graphs at a closed
    point. Nothing is known about control flow. The vertical bar
    stands for "closed point" just as the angle brackets above stand
    for "open point".  Unlike the <*> operator, the |*><*| can create
    a control-flow graph with dangling outedges or unreachable blocks.
    The operator must be used carefully, so we have chosen a long name
    on purpose, to help call people's attention to what they're doing.

  * The operator 'addBlocks' adds a set of basic blocks (represented
    as a closed/closed 'AGraph' to an existing graph, without changing
    the shape of the existing graph.  In some cases, it's necessary to
    introduce a branch and a label to 'get around' the blocks added,
    so this operator, and other functions based on it, requires a
    'HooplNode' type-class constraint and is available only on AGraph,
    not Graph.

  * We have discussed a dynamic assertion about dangling outedges and
    unreachable blocks, but nothing is implemented yet.

-}



class GraphRep g where
  -- | An empty graph that is open at entry and exit.  
  -- It is the left and right identity of '<*>'.
  emptyGraph       :: g n O O
  -- | An empty graph that is closed at entry and exit.  
  -- It is the left and right identity of '|*><*|'.
  emptyClosedGraph :: g n C C
  -- | Create a graph from a first node
  mkFirst  :: n C O -> g n C O
  -- | Create a graph from a middle node
  mkMiddle :: n O O -> g n O O
  -- | Create a graph from a last node
  mkLast   :: n O C -> g n O C
  mkFirst n = mkExit (BlockCO n BNil)
  mkLast  n = mkEntry (BlockOC BNil n)
  infixl 3 <*>
  infixl 2 |*><*| 
  -- | Concatenate two graphs; control flows from left to right.
  (<*>)    :: NonLocal n => g n e O -> g n O x -> g n e x
  -- | Splice together two graphs at a closed point; nothing is known
  -- about control flow.
  (|*><*|) :: NonLocal n => g n e C -> g n C x -> g n e x
  -- | Conveniently concatenate a sequence of open/open graphs using '<*>'.
  catGraphs :: NonLocal n => [g n O O] -> g n O O
  catGraphs = foldr (<*>) emptyGraph

  -- | Create a graph that defines a label
  mkLabel  :: HooplNode n => Label -> g n C O -- definition of the label
  -- | Create a graph that branches to a label
  mkBranch :: HooplNode n => Label -> g n O C -- unconditional branch to the label

  -- | Conveniently concatenate a sequence of middle nodes to form
  -- an open/open graph.
  mkMiddles :: NonLocal n => [n O O] -> g n O O

  mkLabel  id     = mkFirst $ mkLabelNode id
  mkBranch target = mkLast  $ mkBranchNode target
  mkMiddles ms    = catGraphs $ map mkMiddle ms

  -- | Create a graph containing only an entry sequence
  mkEntry   :: Block n O C -> g n O C
  -- | Create a graph containing only an exit sequence
  mkExit    :: Block n C O -> g n C O

instance GraphRep Graph where
  emptyGraph  = GNil
  emptyClosedGraph = GMany NothingO emptyBody NothingO
  (<*>)       = U.gSplice
  (|*><*|)    = U.gSplice
  mkMiddle    = GUnit . BMiddle
  mkExit   block = GMany NothingO      emptyBody (JustO block)
  mkEntry  block = GMany (JustO block) emptyBody NothingO

instance GraphRep AGraph where
  emptyGraph  = aGraphOfGraph emptyGraph
  emptyClosedGraph = aGraphOfGraph emptyClosedGraph
  (<*>)       = liftA2 (<*>)
  (|*><*|)    = liftA2 (|*><*|)
  mkMiddle    = aGraphOfGraph . mkMiddle
  mkExit  = aGraphOfGraph . mkExit
  mkEntry = aGraphOfGraph . mkEntry


-- | The type of abstract graphs.  Offers extra "smart constructors"
-- that may consume fresh labels during construction.
newtype AGraph n e x =
  A { graphOfAGraph :: forall m. UniqueMonad m =>
                       m (Graph n e x) -- ^ Take an abstract 'AGraph'
                                       -- and make a concrete (if monadic)
                                       -- 'Graph'.
    }

-- | Take a graph and make it abstract.
aGraphOfGraph :: Graph n e x -> AGraph n e x
aGraphOfGraph g = A (return g)


-- | The 'Labels' class defines things that can be lambda-bound
-- by an argument to 'withFreshLabels'.  Such an argument may
-- lambda-bind a single 'Label', or if multiple labels are needed,
-- it can bind a tuple.  Tuples can be nested, so arbitrarily many
-- fresh labels can be acquired in a single call.
-- 
-- For example usage see implementations of 'mkIfThenElse' and 'mkWhileDo'.
class Uniques u where
  withFresh :: (u -> AGraph n e x) -> AGraph n e x

instance Uniques Unique where
  withFresh f = A $ freshUnique >>= (graphOfAGraph . f)

instance Uniques Label where
  withFresh f = A $ freshUnique >>= (graphOfAGraph . f . uniqueToLbl)

-- | Lifts binary 'Graph' functions into 'AGraph' functions.
liftA2 :: (Graph  n a b -> Graph  n c d -> Graph  n e f)
       -> (AGraph n a b -> AGraph n c d -> AGraph n e f)
liftA2 f (A g) (A g') = A (liftM2 f g g')

-- | Extend an existing 'AGraph' with extra basic blocks "out of line".
-- No control flow is implied.  Simon PJ should give example use case.
addBlocks      :: HooplNode n
               => AGraph n e x -> AGraph n C C -> AGraph n e x
addBlocks (A g) (A blocks) = A $ g >>= \g -> blocks >>= add g
  where add :: (UniqueMonad m, HooplNode n)
            => Graph n e x -> Graph n C C -> m (Graph n e x)
        add (GMany e body x) (GMany NothingO body' NothingO) =
          return $ GMany e (body `U.bodyUnion` body') x
        add g@GNil      blocks = spliceOO g blocks
        add g@(GUnit _) blocks = spliceOO g blocks
        spliceOO :: (HooplNode n, UniqueMonad m)
                 => Graph n O O -> Graph n C C -> m (Graph n O O)
        spliceOO g blocks = graphOfAGraph $ withFresh $ \l ->
          A (return g) <*> mkBranch l |*><*| A (return blocks) |*><*| mkLabel l

-- | For some graph-construction operations and some optimizations,
-- Hoopl must be able to create control-flow edges using a given node
-- type 'n'.
class NonLocal n => HooplNode n where
  -- | Create a branch node, the source of a control-flow edge.
  mkBranchNode :: Label -> n O C
  -- | Create a label node, the target (destination) of a control-flow edge.
  mkLabelNode  :: Label -> n C O

--------------------------------------------------------------
--                   Shiny Things
--------------------------------------------------------------

class IfThenElseable x where
  -- | Translate a high-level if-then-else construct into an 'AGraph'.
  -- The condition takes as arguments labels on the true-false branch
  -- and returns a single-entry, two-exit graph which exits to 
  -- the two labels.
  mkIfThenElse :: HooplNode n
               => (Label -> Label -> AGraph n O C) -- ^ branch condition
               -> AGraph n O x   -- ^ code in the "then" branch
               -> AGraph n O x   -- ^ code in the "else" branch 
               -> AGraph n O x   -- ^ resulting if-then-else construct

mkWhileDo    :: HooplNode n
             => (Label -> Label -> AGraph n O C) -- ^ loop condition
             -> AGraph n O O -- ^ body of the loop
             -> AGraph n O O -- ^ the final while loop

instance IfThenElseable O where
  mkIfThenElse cbranch tbranch fbranch = withFresh $ \(endif, ltrue, lfalse) ->
    cbranch ltrue lfalse |*><*|
      mkLabel ltrue  <*> tbranch <*> mkBranch endif |*><*|
      mkLabel lfalse <*> fbranch <*> mkBranch endif |*><*|
      mkLabel endif

instance IfThenElseable C where
  mkIfThenElse cbranch tbranch fbranch = withFresh $ \(ltrue, lfalse) ->
    cbranch ltrue lfalse |*><*|
       mkLabel ltrue  <*> tbranch |*><*|
       mkLabel lfalse <*> fbranch

mkWhileDo cbranch body = withFresh $ \(test, head, endwhile) ->
     -- Forrest Baskett's while-loop layout
  mkBranch test |*><*|
    mkLabel head <*> body <*> mkBranch test |*><*|
    mkLabel test <*> cbranch head endwhile  |*><*|
    mkLabel endwhile

--------------------------------------------------------------
--               Boring instance declarations
--------------------------------------------------------------


instance (Uniques u1, Uniques u2) => Uniques (u1, u2) where
  withFresh f = withFresh $ \u1 ->
                withFresh $ \u2 ->
                f (u1, u2)

instance (Uniques u1, Uniques u2, Uniques u3) => Uniques (u1, u2, u3) where
  withFresh f = withFresh $ \u1 ->
                withFresh $ \u2 ->
                withFresh $ \u3 ->
                f (u1, u2, u3)

instance (Uniques u1, Uniques u2, Uniques u3, Uniques u4) => Uniques (u1, u2, u3, u4) where
  withFresh f = withFresh $ \u1 ->
                withFresh $ \u2 ->
                withFresh $ \u3 ->
                withFresh $ \u4 ->
                f (u1, u2, u3, u4)

---------------------------------------------
-- deprecated legacy functions

{-# DEPRECATED addEntrySeq, addExitSeq, unionBlocks "use |*><*| instead" #-}
addEntrySeq :: NonLocal n => AGraph n O C -> AGraph n C x -> AGraph n O x
addExitSeq  :: NonLocal n => AGraph n e C -> AGraph n C O -> AGraph n e O
unionBlocks :: NonLocal n => AGraph n C C -> AGraph n C C -> AGraph n C C

addEntrySeq = (|*><*|)
addExitSeq  = (|*><*|)
unionBlocks = (|*><*|)
