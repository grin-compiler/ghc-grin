{-# LANGUAGE CPP, GADTs #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Compiler.Hoopl.Shape {-# DEPRECATED "not ready to migrate to this yet" #-}
where

-- | Used at the type level to indicate an "open" structure with    
-- a unique, unnamed control-flow edge flowing in or out.         
-- "Fallthrough" and concatenation are permitted at an open point.
data O 
       
       
-- | Used at the type level to indicate a "closed" structure which
-- supports control transfer only through the use of named
-- labels---no "fallthrough" is permitted.  The number of control-flow
-- edges is unconstrained.
data C


data HalfShape s where
  ShapeO :: HalfShape O
  ShapeC :: HalfShape C

data Shape e x where
  ShapeOO :: Shape O O
  ShapeCO :: Shape C O
  ShapeOC :: Shape O C
  ShapeCC :: Shape C C

class Shapely n where
  shape        :: n e x -> Shape e x
  shapeAtEntry :: n e x -> HalfShape e
  shapeAtExit  :: n e x -> HalfShape x

  shapeAtEntry = entryHalfShape . shape
  shapeAtExit  = exitHalfShape  . shape
  

entryHalfShape :: Shape e x -> HalfShape e
exitHalfShape  :: Shape e x -> HalfShape x

entryHalfShape ShapeOO = ShapeO
entryHalfShape ShapeOC = ShapeO
entryHalfShape ShapeCO = ShapeC
entryHalfShape ShapeCC = ShapeC

exitHalfShape ShapeOO = ShapeO
exitHalfShape ShapeOC = ShapeC
exitHalfShape ShapeCO = ShapeO
exitHalfShape ShapeCC = ShapeC

