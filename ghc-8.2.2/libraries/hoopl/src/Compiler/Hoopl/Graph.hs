{-# LANGUAGE CPP, GADTs, TypeFamilies, ScopedTypeVariables,
    RankNTypes, FlexibleInstances, TypeSynonymInstances #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ < 701
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

module Compiler.Hoopl.Graph 
  (
    -- * Body
    Body, Body', emptyBody, bodyList, addBlock, bodyUnion

    -- * Graph
  , Graph, Graph'(..)
  , NonLocal(entryLabel, successors)

  -- ** Constructing graphs
  , bodyGraph
  , blockGraph
  , gUnitOO, gUnitOC, gUnitCO, gUnitCC
  , catGraphNodeOC, catGraphNodeOO
  , catNodeCOGraph, catNodeOOGraph

  -- ** Splicing graphs
  , splice, gSplice

  -- ** Maps
  , mapGraph, mapGraphBlocks

  -- ** Folds
  , foldGraphNodes

  -- ** Extracting Labels
  , labelsDefined, labelsUsed, externalEntryLabels

  -- ** Depth-first traversals
  , postorder_dfs, postorder_dfs_from, postorder_dfs_from_except
  , preorder_dfs, preorder_dfs_from_except
  , LabelsPtr(..)
  )
where

import Compiler.Hoopl.Collections
import Compiler.Hoopl.Block
import Compiler.Hoopl.Label

import Control.Applicative as AP (Applicative(..))
import Control.Monad (ap,liftM,liftM2)

-- -----------------------------------------------------------------------------
-- Body

-- | A (possibly empty) collection of closed/closed blocks
type Body n = LabelMap (Block n C C)

-- | @Body@ abstracted over @block@
type Body' block (n :: * -> * -> *) = LabelMap (block n C C)

emptyBody :: Body' block n
emptyBody = mapEmpty

bodyUnion :: forall a . LabelMap a -> LabelMap a -> LabelMap a
bodyUnion = mapUnionWithKey nodups
  where nodups l _ _ = error $ "duplicate blocks with label " ++ show l

bodyList :: Body' block n -> [(Label,block n C C)]
bodyList body = mapToList body

addBlock :: NonLocal thing
         => thing C C -> LabelMap (thing C C)
         -> LabelMap (thing C C)
addBlock b body
  | mapMember lbl body = error $ "duplicate label " ++ show lbl ++ " in graph"
  | otherwise          = mapInsert lbl b body
  where lbl = entryLabel b


-- ---------------------------------------------------------------------------
-- Graph

-- | A control-flow graph, which may take any of four shapes (O/O,
-- O/C, C/O, C/C).  A graph open at the entry has a single,
-- distinguished, anonymous entry point; if a graph is closed at the
-- entry, its entry point(s) are supplied by a context.
type Graph = Graph' Block

-- | @Graph'@ is abstracted over the block type, so that we can build
-- graphs of annotated blocks for example (Compiler.Hoopl.Dataflow
-- needs this).
data Graph' block (n :: * -> * -> *) e x where
  GNil  :: Graph' block n O O
  GUnit :: block n O O -> Graph' block n O O
  GMany :: MaybeO e (block n O C)
        -> Body' block n
        -> MaybeO x (block n C O)
        -> Graph' block n e x

-------------------------------
-- | Gives access to the anchor points for
-- nonlocal edges as well as the edges themselves
class NonLocal thing where 
  entryLabel :: thing C x -> Label   -- ^ The label of a first node or block
  successors :: thing e C -> [Label] -- ^ Gives control-flow successors

instance NonLocal n => NonLocal (Block n) where
  entryLabel (BlockCO f _)   = entryLabel f
  entryLabel (BlockCC f _ _) = entryLabel f

  successors (BlockOC   _ n) = successors n
  successors (BlockCC _ _ n) = successors n


-- -----------------------------------------------------------------------------
-- Constructing graphs

bodyGraph :: Body n -> Graph n C C
bodyGraph b = GMany NothingO b NothingO

gUnitOO :: block n O O -> Graph' block n O O
gUnitOC :: block n O C -> Graph' block n O C
gUnitCO :: block n C O -> Graph' block n C O
gUnitCC :: NonLocal (block n) => block n C C -> Graph' block n C C
gUnitOO b = GUnit b
gUnitOC b = GMany (JustO b) emptyBody  NothingO
gUnitCO b = GMany NothingO  emptyBody (JustO b)
gUnitCC b = GMany NothingO (addBlock b emptyBody) NothingO


catGraphNodeOO ::               Graph n e O -> n O O -> Graph n e O
catGraphNodeOC :: NonLocal n => Graph n e O -> n O C -> Graph n e C
catNodeOOGraph ::               n O O -> Graph n O x -> Graph n O x
catNodeCOGraph :: NonLocal n => n C O -> Graph n O x -> Graph n C x

catGraphNodeOO GNil                     n = gUnitOO $ BMiddle n
catGraphNodeOO (GUnit b)                n = gUnitOO $ BSnoc b n
catGraphNodeOO (GMany e body (JustO (BlockCO f b))) n
  = GMany e body (JustO (BlockCO f (BSnoc b n)))

catGraphNodeOC GNil                     n = gUnitOC $ BlockOC BNil n
catGraphNodeOC (GUnit b)                n = gUnitOC $ BlockOC b n
catGraphNodeOC (GMany e body (JustO (BlockCO f x))) n
  = GMany e (addBlock (BlockCC f x n) body) NothingO

catNodeOOGraph n GNil                     = gUnitOO $ BMiddle n
catNodeOOGraph n (GUnit b)                = gUnitOO $ BCons n b
catNodeOOGraph n (GMany (JustO (BlockOC b l)) body x)
   = GMany (JustO (BlockOC (n `BCons` b) l)) body x

catNodeCOGraph f GNil                     = gUnitCO (BlockCO f BNil)
catNodeCOGraph f (GUnit b)                = gUnitCO (BlockCO f b)
catNodeCOGraph f (GMany (JustO (BlockOC b n)) body x)
  = GMany NothingO (addBlock (BlockCC f b n) body) x


blockGraph :: NonLocal n => Block n e x -> Graph n e x
blockGraph b@(BlockCO {}) = gUnitCO b
blockGraph b@(BlockOC {}) = gUnitOC b
blockGraph b@(BlockCC {}) = gUnitCC b
blockGraph   (BNil  {})   = GNil
blockGraph b@(BMiddle {}) = gUnitOO b
blockGraph b@(BCat {})    = gUnitOO b
blockGraph b@(BSnoc {})   = gUnitOO b
blockGraph b@(BCons {})   = gUnitOO b


-- -----------------------------------------------------------------------------
-- Splicing graphs

splice :: forall block n e a x . NonLocal (block n) =>
          (forall e x . block n e O -> block n O x -> block n e x)
       -> (Graph' block n e a -> Graph' block n a x -> Graph' block n e x)

splice bcat = sp
  where sp :: forall e a x .
              Graph' block n e a -> Graph' block n a x -> Graph' block n e x

        sp GNil g2 = g2
        sp g1 GNil = g1

        sp (GUnit b1) (GUnit b2) = {-# SCC "sp1" #-} GUnit $! b1 `bcat` b2

        sp (GUnit b) (GMany (JustO e) bs x) = {-# SCC "sp2" #-} GMany (JustO (b `bcat` e)) bs x

        sp (GMany e bs (JustO x)) (GUnit b2) = {-# SCC "sp3" #-} x `seq` GMany e bs (JustO x')
             where x' = x `bcat` b2

        sp (GMany e1 bs1 (JustO x1)) (GMany (JustO e2) b2 x2)
          = {-# SCC "sp4" #-} (GMany e1 $! b1 `bodyUnion` b2) x2
          where b1   = (addBlock $! x1 `bcat` e2) bs1

        sp (GMany e1 b1 NothingO) (GMany NothingO b2 x2)
          = {-# SCC "sp5" #-} (GMany e1 $! b1 `bodyUnion` b2) x2

#if __GLASGOW_HASKELL__ < 711
        sp _ _ = error "bogus GADT match failure"
#endif

gSplice :: NonLocal n => Graph n e a -> Graph n a x -> Graph n e x
gSplice = splice blockAppend


-- -----------------------------------------------------------------------------
-- Mapping over graphs

-- | Maps over all nodes in a graph.
mapGraph :: (forall e x. n e x -> n' e x) -> Graph n e x -> Graph n' e x
mapGraph f = mapGraphBlocks (mapBlock f)

-- | Function 'mapGraphBlocks' enables a change of representation of blocks,
-- nodes, or both.  It lifts a polymorphic block transform into a polymorphic
-- graph transform.  When the block representation stabilizes, a similar
-- function should be provided for blocks.
mapGraphBlocks :: forall block n block' n' e x .
                  (forall e x . block n e x -> block' n' e x)
               -> (Graph' block n e x -> Graph' block' n' e x)

mapGraphBlocks f = map
  where map :: Graph' block n e x -> Graph' block' n' e x
        map GNil = GNil
        map (GUnit b) = GUnit (f b)
        map (GMany e b x) = GMany (fmap f e) (mapMap f b) (fmap f x)


-- -----------------------------------------------------------------------------
-- Folds

-- | Fold a function over every node in a graph.
-- The fold function must be polymorphic in the shape of the nodes.

foldGraphNodes :: forall n a .
                  (forall e x . n e x       -> a -> a)
               -> (forall e x . Graph n e x -> a -> a)

foldGraphNodes f = graph
    where graph :: forall e x . Graph n e x -> a -> a
          lift  :: forall thing ex . (thing -> a -> a) -> (MaybeO ex thing -> a -> a)

          graph GNil              = id
          graph (GUnit b)         = block b
          graph (GMany e b x)     = lift block e . body b . lift block x
          body :: Body n -> a -> a
          body bdy                = \a -> mapFold block a bdy
          lift _ NothingO         = id
          lift f (JustO thing)    = f thing

          block :: Block n e x -> IndexedCO e a a -> IndexedCO x a a
          block = foldBlockNodesF f


----------------------------------------------------------------

class LabelsPtr l where
  targetLabels :: l -> [Label]

instance NonLocal n => LabelsPtr (n e C) where
  targetLabels n = successors n

instance LabelsPtr Label where
  targetLabels l = [l]

instance LabelsPtr LabelSet where
  targetLabels = setElems

instance LabelsPtr l => LabelsPtr [l] where
  targetLabels = concatMap targetLabels


-- | Traversal: 'postorder_dfs' returns a list of blocks reachable
-- from the entry of enterable graph. The entry and exit are *not* included.
-- The list has the following property:
--
--	Say a "back reference" exists if one of a block's
--	control-flow successors precedes it in the output list
--
--	Then there are as few back references as possible
--
-- The output is suitable for use in
-- a forward dataflow problem.  For a backward problem, simply reverse
-- the list.  ('postorder_dfs' is sufficiently tricky to implement that
-- one doesn't want to try and maintain both forward and backward
-- versions.)

postorder_dfs :: NonLocal (block n) => Graph' block n O x -> [block n C C]
preorder_dfs  :: NonLocal (block n) => Graph' block n O x -> [block n C C]

-- | This is the most important traversal over this data structure.  It drops
-- unreachable code and puts blocks in an order that is good for solving forward
-- dataflow problems quickly.  The reverse order is good for solving backward
-- dataflow problems quickly.  The forward order is also reasonably good for
-- emitting instructions, except that it will not usually exploit Forrest
-- Baskett's trick of eliminating the unconditional branch from a loop.  For
-- that you would need a more serious analysis, probably based on dominators, to
-- identify loop headers.
--
-- The ubiquity of 'postorder_dfs' is one reason for the ubiquity of the 'LGraph'
-- representation, when for most purposes the plain 'Graph' representation is
-- more mathematically elegant (but results in more complicated code).
--
-- Here's an easy way to go wrong!  Consider
-- @
--	A -> [B,C]
--	B -> D
--	C -> D
-- @
-- Then ordinary dfs would give [A,B,D,C] which has a back ref from C to D.
-- Better to get [A,B,C,D]


graphDfs :: (LabelMap (block n C C) -> block n O C -> LabelSet -> [block n C C])
         -> (Graph' block n O x -> [block n C C])
graphDfs _     (GNil)    = []
graphDfs _     (GUnit{}) = []
graphDfs order (GMany (JustO entry) body _) = order body entry setEmpty

postorder_dfs = graphDfs postorder_dfs_from_except
preorder_dfs  = graphDfs preorder_dfs_from_except

postorder_dfs_from_except :: forall block e . (NonLocal block, LabelsPtr e)
                          => LabelMap (block C C) -> e -> LabelSet -> [block C C]
postorder_dfs_from_except blocks b visited =
 vchildren (get_children b) (\acc _visited -> acc) [] visited
 where
   vnode :: block C C -> ([block C C] -> LabelSet -> a) -> [block C C] -> LabelSet -> a
   vnode block cont acc visited =
        if setMember id visited then
            cont acc visited
        else
            let cont' acc visited = cont (block:acc) visited in
            vchildren (get_children block) cont' acc (setInsert id visited)
      where id = entryLabel block
   vchildren :: forall a. [block C C] -> ([block C C] -> LabelSet -> a) -> [block C C] -> LabelSet -> a
   vchildren bs cont acc visited = next bs acc visited
      where next children acc visited =
                case children of []     -> cont acc visited
                                 (b:bs) -> vnode b (next bs) acc visited
   get_children :: forall l. LabelsPtr l => l -> [block C C]
   get_children block = foldr add_id [] $ targetLabels block
   add_id id rst = case lookupFact id blocks of
                      Just b -> b : rst
                      Nothing -> rst

postorder_dfs_from
    :: (NonLocal block, LabelsPtr b) => LabelMap (block C C) -> b -> [block C C]
postorder_dfs_from blocks b = postorder_dfs_from_except blocks b setEmpty


----------------------------------------------------------------

data VM a = VM { unVM :: LabelSet -> (a, LabelSet) }

instance Functor VM where
  fmap  = liftM

instance Applicative VM where
  pure a = VM $ \visited -> (a, visited)
  (<*>) = ap

instance Monad VM where
  return = AP.pure
  m >>= k  = VM $ \visited -> let (a, v') = unVM m visited in unVM (k a) v'

marked :: Label -> VM Bool
marked l = VM $ \v -> (setMember l v, v)

mark   :: Label -> VM ()
mark   l = VM $ \v -> ((), setInsert l v)

preorder_dfs_from_except :: forall block e . (NonLocal block, LabelsPtr e)
                         => LabelMap (block C C) -> e -> LabelSet -> [block C C]
preorder_dfs_from_except blocks b visited =
    (fst $ unVM (children (get_children b)) visited) []
  where children [] = return id
        children (b:bs) = liftM2 (.) (visit b) (children bs)
        visit :: block C C -> VM (HL (block C C))
        visit b = do already <- marked (entryLabel b)
                     if already then return id
                      else do mark (entryLabel b)
                              bs <- children $ get_children b
                              return $ b `cons` bs
        get_children :: forall l. LabelsPtr l => l -> [block C C]
        get_children block = foldr add_id [] $ targetLabels block

        add_id id rst = case lookupFact id blocks of
                          Just b -> b : rst
                          Nothing -> rst

type HL a = [a] -> [a] -- Hughes list (constant-time concatenation)
cons :: a -> HL a -> HL a
cons a as tail = a : as tail


-- -----------------------------------------------------------------------------
-- Extracting Labels from graphs

labelsDefined :: forall block n e x . NonLocal (block n) => Graph' block n e x
              -> LabelSet
labelsDefined GNil      = setEmpty
labelsDefined (GUnit{}) = setEmpty
labelsDefined (GMany _ body x) = mapFoldWithKey addEntry (exitLabel x) body
  where addEntry :: forall a. ElemOf LabelSet -> a -> LabelSet -> LabelSet
        addEntry label _ labels = setInsert label labels
        exitLabel :: MaybeO x (block n C O) -> LabelSet
        exitLabel NothingO  = setEmpty
        exitLabel (JustO b) = setSingleton (entryLabel b)

labelsUsed :: forall block n e x. NonLocal (block n) => Graph' block n e x
           -> LabelSet
labelsUsed GNil      = setEmpty
labelsUsed (GUnit{}) = setEmpty
labelsUsed (GMany e body _) = mapFold addTargets (entryTargets e) body 
  where addTargets :: forall e. block n e C -> LabelSet -> LabelSet
        addTargets block labels = setInsertList (successors block) labels
        entryTargets :: MaybeO e (block n O C) -> LabelSet
        entryTargets NothingO = setEmpty
        entryTargets (JustO b) = addTargets b setEmpty

externalEntryLabels :: forall n .
                       NonLocal n => LabelMap (Block n C C) -> LabelSet
externalEntryLabels body = defined `setDifference` used
  where defined = labelsDefined g
        used = labelsUsed g
        g = GMany NothingO body NothingO

