# Changelog for [`hoopl` package](http://hackage.haskell.org/package/hoopl)

## 3.10.2.2 *Feb 2017*
  This release includes non-API changes.

 - Use cabal builtin options to enable test coverage

 - Move up the constraints of base and containers

 - Refactor the references of the fold family functions to their equivalant foldr functions.

 - Drop the support for 7.0.1

 - Fix a bug that drops out dominators when joined DPATHs have non-shared nodes in the middle.


## 3.10.2.1 *Dec 2015*
  This release includes only non-functional changes.

 - Rewrite Applicative/Monad instances into normal-form

 - Relax the upper bound constraint of base to include 4.9

 - Replace `#if CABAL` macro by no CPP at all

 - Wrap redudant wild card pattens in conditional compilation 

 - Prefix unused type variables with underscores. 

## 3.10.2.0 *Aug 2015*

 - Add #if CABAL macro to several hoopl source files such that the Cabal generated macro is not included when building in ghci

 - Change the test code (testing/*) to compare the converted graphs against the expected graphs in AST form 

 - Update the cabal file to run tests and generate a test coverage report 
 
 - Unhide gSplice of Hoopl.Graph

 - Expose Showing of Hoopl.Show

 - Some fixes of testing

## 3.10.1.0 *Apr 2015*

 - Re-export runWithFuel from Compiler.Hoopl.

 - Remove redundant constraints

## 3.10.0.2  *Dec 2014*

 - Add support for `base-4.8.0.0` package version

 - Mark a few modules as Safe rather than Trustworthy

## 3.10.0.1  *Mar 2014*

  - Remove UTF8 character from hoopl.cabal to workaround issue
    in GHC 7.8.1's build system

## 3.10.0.0  *Mar 2014*

  - Bundled with GHC 7.8.1

  - Define `Functor` and `Applicative` instances for
     - `SimpleUniqueMonad`
     - `CheckingFuelMonad`
     - `InfiniteFuelMonad`
     - `UniqueMonadT`
     - `VM` (not exported)

  - Update to Cabal format 1.10

## 3.9.0.0

Lots of API changes; mainly a new API for working with Blocks

### Summary of refactorings:

   - `Compiler.Hoopl.Block` contains the `Block` datatype and all the
     operations on Blocks.  It seemed like a good idea to collect all
     this stuff together in one place.

   - `Compiler.Hoopl.Graph` now has the operations on Graphs.

   - `Compiler.Hoopl.Util` and `Compiler.Hoopl.GraphUtil` are no more; their
     contents have been moved to other homes.  (and a bit of duplicated
     functionality has been removed).

   - I removed the newtypes around `Unique` and `Label`, these are now just
     type synonyms.  The newtype wrappers were costing some performance in
     GHC, because in cases like `mapToList` the newtype isn't optimised away.

     This change might be controversial.  Feel free to complain.

### Other changes:

   - Optimisations to the Dataflow algorithms.  I'm not actually using
     this implementation of Dataflow in GHC any more, instead I have a
     local copy specialised to our monad, for speed.  Nevertheless I've
     put some of the optimisations I'm using in the GHC version into the
     generic library version too.

### Summary of API changes:

#### Added

  - `IsMap(mapInsertWith, mapFromListWith)`

  - `mapGraphBlocks`
    (was previously called `graphMapBlocks`, and not exported)

  - `mapBlock'`
    (strict version of `mapBlock`)

  - New API for working with Blocks:

    ```haskell
      -- ** Predicates on Blocks
    , isEmptyBlock
 
      -- ** Constructing blocks
    , emptyBlock, blockCons, blockSnoc
    , blockJoinHead, blockJoinTail, blockJoin, blockJoinAny
    , blockAppend
 
      -- ** Deconstructing blocks
    , firstNode, lastNode, endNodes
    , blockSplitHead, blockSplitTail, blockSplit, blockSplitAny
 
      -- ** Modifying blocks
    , replaceFirstNode, replaceLastNode
 
      -- ** Converting to and from lists
    , blockToList, blockFromList
 
      -- ** Maps and folds
    , mapBlock, mapBlock', mapBlock3'
    , foldBlockNodesF, foldBlockNodesF3
    , foldBlockNodesB, foldBlockNodesB3
    ```

#### Removed

  - `mapMaybeO`, `mapMaybeC`
    (no need: we have `Functor` instances)

  - Block constructors are no longer exported
    (use the block API instead)

  - `blockToNodeList`, `blockToNodeList'`, `blockToNodeList''`, `blockToNodeList'''`
    (use the block API instead)

  - `tfFoldBlock`, `ScottBlock`, `scottFoldBlock`, `fbnf3`,
    `BlockResult(..)`, `lookupBlock`,
    (I don't know what any of these are for, if they're still important
    we could reinstate)

#### Changed

  - Compiler.Hoopl.GHC is now Compiler.Hoopl.Internals and exports some
    more stuff.

  - Label is not a newtype; type Label = Unique
  - Unique is not a newtype: type Unique = Int
    (these newtypes were adding overhead)

  - blockMapNodes3 is now mapBlock3'

  - Lots of internal refactoring and tidying up

## 3.8.7.4

  - Re-export runWithFuel as per Justin Bailey

## 3.8.7.3

  - Uploaded to Hackage by Ian Lynagh; appears to contain updates
    that use Safe Haskell if GHC >= 7.2 (thanks David Terei)

## 3.8.7.2

  - Version changed with no record of update; never uploaded to Hackage

## 3.8.7.1

  - Eliminate warning about nonexhaustive pattern match (thanks Edward Yang)

## 3.8.7.0

  - Works with GHC 7 (thanks Edward Yang)
  - `cabal sdist` now sort of works (and is added to validate)

## 3.8.6.0

  - Matches the camera-ready Haskell'10 paper

## 3.8.1.0

  - Major reorganization per simonpj visit to Tufts 20 April 2010
    Collections

## 3.7.13.1

  - Added function to fold over nodes in graph.

## 3.7.13.0

  - Pointed type replaces WithTop and WithBot, which are now synonyms.

## 3.7.12.3

### Interface changes

The type of AGraph is now abstract.
It is now recommended to create AGraphs with just three functions:

    <*>          concatenation
    |*><*|       splicing at a closed point
    addBlocks    add C/C blocks out of line

There are new utility functions in modules Util and XUtil, all
exported by Compiler.Hoopl.  Here's a selection:

    -- | A utility function so that a transfer function for a first
    -- node can be given just a fact; we handle the lookup.  This
    -- function is planned to be made obsolete by changes in the dataflow
    -- interface.
    firstXfer :: Edges n => (n C O -> f -> f) -> (n C O -> FactBase f -> f)
    firstXfer xfer n fb = xfer n $ fromJust $ lookupFact fb $ entryLabel n

    -- | This utility function handles a common case in which a transfer function
    -- produces a single fact out of a last node, which is then distributed
    -- over the outgoing edges.
    distributeXfer :: Edges n => (n O C -> f -> f) -> (n O C -> f -> FactBase f)
    distributeXfer xfer n f = mkFactBase [ (l, xfer n f) | l <- successors n ]

    -- | This utility function handles a common case in which a transfer function
    -- for a last node takes the incoming fact unchanged and simply distributes
    -- that fact over the outgoing edges.
    distributeFact :: Edges n => n O C -> f -> FactBase f

    -- | Function 'graphMapBlocks' enables a change of representation of blocks,
    -- nodes, or both.  It lifts a polymorphic block transform into a polymorphic
    -- graph transform.  When the block representation stabilizes, a similar
    -- function should be provided for blocks.
    graphMapBlocks :: forall block n block' n' e x .
                      (forall e x . block n e x -> block' n' e x)
                   -> (Graph' block n e x -> Graph' block' n' e x)

    postorder_dfs :: Edges (block n) => Graph' block n O x -> [block n C C]

There are quite a few other variations related to depth-first traversal.


There is a new module Compiler.Hoopl.Pointed, which uses GADTs to
enable you to add a Top or Bot element to a lattice, or both, all
using the same type.  (Types `WithBot` and `WithTop` in XUtil,
exported by Compiler.Hoopl, do similar jobs, but I think they are
inferior.  Your opinion is solicited.)

I added a function showGraph to print Graphs.  Right now it requires a
polymorphic node-showing function as argument. When we change the
Block representation we may get enough static type information that we
can simply have an instance declaration for

    instance (Show n C O, Show n O O, Show n O C) => Show (Graph n e x)

At present, I don't see how to achieve such a declaration.

John added new functions `debugFwdJoins` and `debugBwdJoins` to extend
passes with debugging information.

We added primed versions of the analyzeAndRewrite functions, which
operate on Graph, not Body.  Recommended, at least for first-timers.

Not all maps keyed by Label are FactBases, so there is now a new set
of names of functions to manipulate LabelMaps that do not contain
dataflow facts.

### Implementation changes

Split pass and rewrite-function combinators into Compiler.Hoopl.Combinators.

Changed order of blocks for forward and backward analysis.
These changes have not been tested, because we don't have a true
regression suite yet.

Graph and Body types now have more polymorphic variants Graph' and Body'.

Lots of experiments with zippers.

### Changes ahead

ForwardTransfer will become an abstract type, and clients will have
two ways to create ForwardTransfers: as now, with a single,
polymorphic transfer function; or with a triple of monomorphic
transfer functions.  The implementation will use monomorphic
functions, which will enable more useful combinators on passes,
including adding more debugging information and enabling us to combine
passes.

Perhaps we should provide splicing and `addBlocks` on Graph?

Change of block representation to have three monomorphic unit
constructors and one polymorphic concatenation constructor.

Graph body to be represented by a finite map; add functions to check
for duplicate labels.

## 3.7.12.1

  - Added a bunch of zipper experiments.
  - Existing clients should not be affected.

## 3.7.12.0

  - More expressive debugging support
  - retract arfGraph and normalization; export analyzeAndRewriterFwd'

## 3.7.11.1

  - Expose arfGraph and normalization functions

## 3.7.11.0

  - Debugging support

## 3.7.8.0

  - Rationalized AGraph splicing functions.

## 3.7.7.0

  - Restrict clients so they see much less, including hiding
    the value constructors for Body.
