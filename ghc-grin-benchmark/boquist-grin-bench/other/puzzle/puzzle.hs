module Main where

-- hbcc prelude:
#ifdef __HBCC__
#include "prel.hs"
#endif

main = print ex4b

type Point = (Int, Int)         -- relative OR absolute coordinates

-- a block:
type Block = [Point]            -- relative coordinates (0,-1,+1)
type Cover = [Point]            -- absolute coordinates

pointLT :: Point -> Point -> Bool
pointLT (x1,y1) (x2,y2) = x1 < x2 || x1 == x2 && y1 < y2

pointEQ :: Point -> Point -> Bool
pointEQ (x1,y1) (x2,y2) = x1 == x2 && y1 == y2

coverEQ :: Cover -> Cover -> Bool
coverEQ [] [] = True
coverEQ [] (_:_) = False
coverEQ (_:_) [] = False
coverEQ (x:xs) (y:ys) = x `pointEQ` y && xs `coverEQ` ys

coverElem :: Point -> Cover -> Bool
coverElem p [] = False
coverElem p (q:qs) = p `pointEQ` q || p `coverElem` qs

-- search state:
type State = (Cover,            -- filled sofar (always sorted!)
              [(Block,Cover)],  -- already placed blocks (history)
              [Block])          -- rest of blocks

-- all the ways to "transform" a block:
rotate90 :: Block -> Block
rotate90 = map rot
    where rot (x,y) = (-y,x)

rotate180 :: Block -> Block
rotate180 = rotate90 . rotate90

rotate270 :: Block -> Block
rotate270 = rotate90 . rotate180

mirror :: Block -> Block
mirror = map mirr
    where mirr (x,y) = (x,-y)

-- generate all variations of a block:
allBlocks :: Block -> [Block]
allBlocks p = [p, rotate90 p, rotate180 p, rotate270 p] ++
              [q, rotate90 q, rotate180 q, rotate270 q]
    where q = mirror p

addPoint :: Point -> Point -> Point
addPoint (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-- insert new Points into a (sorted) Cover (and skip duplicates):
insert :: [Point] -> Cover -> Cover
insert new old = foldr insert1 old new
    where insert1 p [] = [p]
          insert1 p css@(c:cs) = if p `pointLT` c  then p : css else
				 if p `pointEQ` c then css else
				 c : insert1 p cs

-- put a block on the board.
-- make sure that the resulting Cover contains no duplicate points:
walk :: Point -> Block -> Cover
walk start = walk' [start]
    where walk' ss [] = insert ss []
          walk' ss@(p1:_) (p2:ps) = walk' (addPoint p1 p2 : ss) ps

-- put a block in all possible rotations:
allWalks :: Point -> Block -> [Cover]
allWalks start = map (walk start) . allBlocks

-- all legal ways to put a new block:
allOkWalks :: (Int,Int) -> Cover -> Point -> Block -> [Cover]
allOkWalks (xMax,yMax) cover start b = filter (all ok) (allWalks start b)
    where ok (x,y) = x > 0 && x <= xMax &&
                     y > 0 && y <= yMax &&
                     not ((x,y) `coverElem` cover)

-- try all different rotations + free starting points
-- for the next block, or skip it:
putNextBlock :: (Int,Int) -> [Point] -> State -> [State]
putNextBlock _ _ (_,_,[]) = []
putNextBlock bounds allPoints (cover, old, b:bs) =
    -- skip this block:
    [ (cover,old,bs) ] ++
    -- add this block:
    [ (insert p cover, (b,p) : old, bs)
      | start <- freePoints allPoints cover,
        p <- allOkWalks bounds cover start b ]

-- "set complement" given a full set and a subset (both must be sorted):
freePoints :: Cover -> Cover -> Cover
freePoints (p:ps) qss@(q:qs) = if p `pointEQ` q then freePoints ps qs
                               else p : freePoints ps qss
freePoints ps [] = ps
freePoints [] qs = error "freePoints"

-- check if a covering is complete:
isFinal :: Cover -> State -> Bool
isFinal completeCover (cover,_,_) = cover `coverEQ` completeCover

--  depth first search, return all states satisfying the final condition:
search :: (State -> [State]) -> (State -> Bool) -> [State] -> [State]
search next final [] = []
search next final (s:ss) = if final s then
                               s : search next final ss
                           else
                               search next final (next s ++ ss)

-- given a size and some blocks, return all solutions to the puzzle:
solve :: (Int,Int) -> [Block] -> [State]
solve bounds@(xMax,yMax) blocks = search (putNextBlock bounds allPoints)
                                         (isFinal allPoints) [initState]
    where allPoints = [ (x,y) | x <- (upto 1 xMax), y <- (upto 1 yMax) ]
          initState = ([],[],blocks)

upto :: Int -> Int -> [Int]
upto m n = if m > n then
	       []
	   else
	       m : upto (m+1) n

-------------------------------------------------------------------------------
-- size 2 (4 solutions):
b1 :: Block
b1 = [(0,-1),(1,0),(1,0),(1,0)]

ex2 = length (solve (5,2) [b1,b1])

-------------------------------------------------------------------------------
-- size 3 (8 solutions):
b2 :: Block; b3 :: Block
b2 = [(1,0),(0,-1),(1,0),(1,0)]
b3 = [(1,0),(1,0),(0,-1),(0,-1)]

ex3 = length (solve (5,3) [b1,b2,b3])

-------------------------------------------------------------------------------
-- size 4 (64 solutions):
b4 :: Block
b4 = [(1,0),(1,0),(1,0),(1,0)]

ex4 = length (solve (5,4) [b1,b2,b3,b4])

-------------------------------------------------------------------------------
-- size 4b (1 extra block,  128 solutions):
b5 :: Block
b5 = [(1,0),(1,0),(0,1),(-1,0)]

ex4b = length (solve (5,4) [b1,b2,b5,b3,b4])

-------------------------------------------------------------------------------
-- size 5 (2 new blocks, ? solutions):
b6 :: Block
b6 = [(1,0),(0,1),(1,0),(0,1)]
b7 :: Block
b7 = [(1,0),(1,0),(0,1),(0,-1),(0,-1)]

ex5 = length (solve (5,5) [b1,b2,b5,b3,b4,b6,b7])
