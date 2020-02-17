{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
#if !(MIN_VERSION_base(4,11,0))
{-# LANGUAGE Trustworthy #-}
#endif

-- | This module was written based on
--   <http://hackage.haskell.org/package/reinterpret-cast-0.1.0/docs/src/Data-ReinterpretCast-Internal-ImplArray.html>.
--
--   Implements casting via a 1-element STUArray, as described in
--   <http://stackoverflow.com/a/7002812/263061>.
module Data.Binary.FloatCast
  ( floatToWord  -- | Reinterpret-casts a `Float` to a `Word32`.
  , wordToFloat  -- | Reinterpret-casts a `Word32` to a `Float`.
  , doubleToWord -- | Reinterpret-casts a `Double` to a `Word64`.
  , wordToDouble -- | Reinterpret-casts a `Word64` to a `Double`.

  ) where

#if MIN_VERSION_base(4,11,0)

import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castFloatToWord32, castWord64ToDouble, castDoubleToWord64)

floatToWord :: Float -> Word32
floatToWord = castFloatToWord32
{-# INLINE floatToWord #-}

wordToFloat :: Word32 -> Float
wordToFloat = castWord32ToFloat
{-# INLINE wordToFloat #-}

doubleToWord :: Double -> Word64
doubleToWord = castDoubleToWord64
{-# INLINE doubleToWord #-}

wordToDouble :: Word64 -> Double
wordToDouble = castWord64ToDouble
{-# INLINE wordToDouble #-}

#else
import Data.Word (Word32, Word64)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)
{-# INLINE floatToWord #-}

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)
{-# INLINE wordToFloat #-}

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)
{-# INLINE doubleToWord #-}

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)
{-# INLINE wordToDouble #-}

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
{-# INLINE cast #-}
#endif
