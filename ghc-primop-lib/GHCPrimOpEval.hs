module GHCPrimOpEval where

import Data.Int
import Data.Word
import Data.Bits
import GHCPrimOp


evalPrimOp :: PrimOp -> [Value] -> Value

-- Char
evalPrimOp CharGtOp [CharV a, CharV b] = IntV $ if a > b  then 1 else 0
evalPrimOp CharGeOp [CharV a, CharV b] = IntV $ if a >= b then 1 else 0
evalPrimOp CharEqOp [CharV a, CharV b] = IntV $ if a == b then 1 else 0
evalPrimOp CharNeOp [CharV a, CharV b] = IntV $ if a /= b then 1 else 0
evalPrimOp CharLtOp [CharV a, CharV b] = IntV $ if a < b  then 1 else 0
evalPrimOp CharLeOp [CharV a, CharV b] = IntV $ if a <= b then 1 else 0
evalPrimOp OrdOp    [CharV a] = IntV $ fromIntegral a -- HINT: noop ; same bit level representation

-- Int
evalPrimOp IntAddOp         [IntV a, IntV b] = IntV $ a + b
evalPrimOp IntSubOp         [IntV a, IntV b] = IntV $ a - b
evalPrimOp IntMulOp         [IntV a, IntV b] = IntV $ a * b
evalPrimOp IntMulMayOfloOp  [IntV a, IntV b] = IntV $ if fromIntegral a * (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimInt) then 1 else 0
evalPrimOp IntQuotOp        [IntV a, IntV b] = IntV $ a `quot` b  -- NOTE: int / int in C
evalPrimOp IntRemOp         [IntV a, IntV b] = IntV $ a `rem` b   -- NOTE: int % int in C
evalPrimOp IntQuotRemOp     [IntV a, IntV b] = TupleV [IntV $ a `quot` b, IntV $ a `rem` b]
evalPrimOp AndIOp           [IntV a, IntV b] = IntV $ a .&. b
evalPrimOp OrIOp            [IntV a, IntV b] = IntV $ a .|. b
evalPrimOp XorIOp           [IntV a, IntV b] = IntV $ a `xor` b
evalPrimOp NotIOp           [IntV a] = IntV $ complement a
evalPrimOp IntNegOp         [IntV a] = IntV (-a)
evalPrimOp IntAddCOp [IntV a, IntV b] = TupleV
                                        [ IntV $ a + b
                                        , IntV $ if fromIntegral a + (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimInt) then 1 else 0
                                        ]
evalPrimOp IntSubCOp [IntV a, IntV b] = TupleV
                                        [ IntV $ a + b
                                        , IntV $ if fromIntegral a - (fromIntegral b :: Integer) < fromIntegral (minBound :: PrimInt) then 1 else 0
                                        ]
evalPrimOp IntGtOp  [IntV a, IntV b] = IntV $ if a > b  then 1 else 0
evalPrimOp IntGeOp  [IntV a, IntV b] = IntV $ if a >= b then 1 else 0
evalPrimOp IntEqOp  [IntV a, IntV b] = IntV $ if a == b then 1 else 0
evalPrimOp IntNeOp  [IntV a, IntV b] = IntV $ if a /= b then 1 else 0
evalPrimOp IntLtOp  [IntV a, IntV b] = IntV $ if a < b  then 1 else 0
evalPrimOp IntLeOp  [IntV a, IntV b] = IntV $ if a <= b then 1 else 0
evalPrimOp ChrOp          [IntV a]  = CharV $ fromIntegral a -- HINT: noop ; same bit level representation
evalPrimOp Int2WordOp     [IntV a]  = WordV $ fromIntegral a -- HINT: noop ; same bit level representation
evalPrimOp Int2FloatOp    [IntV a]  = FloatV $ fromIntegral a
evalPrimOp Int2DoubleOp   [IntV a]  = DoubleV $ fromIntegral a
evalPrimOp Word2FloatOp   [WordV a] = FloatV $ fromIntegral a
evalPrimOp Word2DoubleOp  [WordV a] = DoubleV $ fromIntegral a
evalPrimOp ISllOp [IntV a, IntV b] = IntV $ unsafeShiftL a (fromIntegral b)
evalPrimOp ISraOp [IntV a, IntV b] = IntV $ unsafeShiftR a (fromIntegral b) -- Shift right arithmetic
evalPrimOp ISrlOp [IntV a, IntV b] = IntV $ fromIntegral $ unsafeShiftR (fromIntegral a :: PrimWord) (fromIntegral b) -- Shift right logical

-- Word
evalPrimOp WordAddOp  [WordV a, WordV b] = WordV $ a + b
evalPrimOp WordAddCOp [WordV a, WordV b] = TupleV
                                        [ WordV $ a + b
                                        , IntV $ if fromIntegral a + (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimWord) then 1 else 0
                                        ]
evalPrimOp WordSubCOp [WordV a, WordV b] = TupleV
                                        [ WordV $ a + b
                                        , IntV $ if fromIntegral a - (fromIntegral b :: Integer) < fromIntegral (minBound :: PrimWord) then 1 else 0
                                        ]
evalPrimOp WordAdd2Op [WordV a, WordV b] = TupleV [WordV (fromIntegral hi), WordV (fromIntegral lo)] where
  res = fromIntegral a + fromIntegral b :: Integer
  hi  = res `quot` fromIntegral (1 + maxBound :: PrimWord)
  lo  = res - hi

evalPrimOp WordSubOp  [WordV a, WordV b] = WordV $ a - b
evalPrimOp WordMulOp  [WordV a, WordV b] = WordV $ a * b
evalPrimOp WordMul2Op [WordV a, WordV b] = TupleV [WordV (fromIntegral hi), WordV (fromIntegral lo)] where
  res = fromIntegral a * fromIntegral b :: Integer
  hi  = res `quot` fromIntegral (1 + maxBound :: PrimWord)
  lo  = res - hi

evalPrimOp WordQuotOp     [WordV a, WordV b] = WordV $ a `quot` b  -- NOTE: uint / uint in C
evalPrimOp WordRemOp      [WordV a, WordV b] = WordV $ a `rem` b   -- NOTE: uint % uint in C
evalPrimOp WordQuotRemOp  [WordV a, WordV b] = TupleV [WordV $ a `quot` b, WordV $ a `rem` b]
evalPrimOp WordQuotRem2Op [WordV hi, WordV lo, WordV b'] = TupleV [WordV . fromIntegral $ a `quot` b, WordV . fromIntegral $ a `rem` b] where
  a = fromIntegral hi * fromIntegral (1 + maxBound :: PrimWord) + fromIntegral lo :: Integer
  b = fromIntegral b' :: Integer

evalPrimOp AndOp  [WordV a, WordV b] = WordV $ a .&. b
evalPrimOp OrOp   [WordV a, WordV b] = WordV $ a .|. b
evalPrimOp XorOp  [WordV a, WordV b] = WordV $ a `xor` b
evalPrimOp NotOp  [WordV a] = WordV $ complement a
evalPrimOp SllOp  [WordV a, IntV b] = WordV $ unsafeShiftL a (fromIntegral b)
evalPrimOp SrlOp  [WordV a, IntV b] = WordV $ unsafeShiftR a (fromIntegral b) -- Shift right logical
evalPrimOp Word2IntOp [WordV a] = IntV $ fromIntegral a -- HINT: noop ; same bit level representation
evalPrimOp WordGtOp   [WordV a, WordV b] = IntV $ if a > b  then 1 else 0
evalPrimOp WordGeOp   [WordV a, WordV b] = IntV $ if a >= b then 1 else 0
evalPrimOp WordEqOp   [WordV a, WordV b] = IntV $ if a == b then 1 else 0
evalPrimOp WordNeOp   [WordV a, WordV b] = IntV $ if a /= b then 1 else 0
evalPrimOp WordLtOp   [WordV a, WordV b] = IntV $ if a < b  then 1 else 0
evalPrimOp WordLeOp   [WordV a, WordV b] = IntV $ if a <= b then 1 else 0

evalPrimOp PopCnt8Op  [WordV a] = WordV . fromIntegral $ popCount (fromIntegral a :: Word8)
evalPrimOp PopCnt16Op [WordV a] = WordV . fromIntegral $ popCount (fromIntegral a :: Word16)
evalPrimOp PopCnt32Op [WordV a] = WordV . fromIntegral $ popCount (fromIntegral a :: Word32)
evalPrimOp PopCnt64Op [WordV a] = WordV . fromIntegral $ popCount (fromIntegral a :: Word64)
evalPrimOp PopCntOp   [WordV a] = WordV . fromIntegral $ popCount a

{-
  HINT:
    https://en.wikipedia.org/wiki/Bit_Manipulation_Instruction_Sets#Parallel_bit_deposit_and_extract
    https://www.felixcloutier.com/x86/pdep
-}

{-
  Pdep8Op
  Pdep16Op
  Pdep32Op
  Pdep64Op
  PdepOp
  Pext8Op
  Pext16Op
  Pext32Op
  Pext64Op
  PextOp
-}
evalPrimOp Clz8Op   [WordV a] = WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word8)
evalPrimOp Clz16Op  [WordV a] = WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word16)
evalPrimOp Clz32Op  [WordV a] = WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word32)
evalPrimOp Clz64Op  [WordV a] = WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word64)
evalPrimOp ClzOp    [WordV a] = WordV . fromIntegral $ countLeadingZeros a
evalPrimOp Ctz8Op   [WordV a] = WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word8)
evalPrimOp Ctz16Op  [WordV a] = WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word16)
evalPrimOp Ctz32Op  [WordV a] = WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word32)
evalPrimOp Ctz64Op  [WordV a] = WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word64)
evalPrimOp CtzOp    [WordV a] = WordV . fromIntegral $ countTrailingZeros a

evalPrimOp BSwap16Op  [WordV a] = WordV . fromIntegral $ byteSwap16 (fromIntegral a :: Word16)
evalPrimOp BSwap32Op  [WordV a] = WordV . fromIntegral $ byteSwap32 (fromIntegral a :: Word32)
evalPrimOp BSwap64Op  [WordV a] = WordV . fromIntegral $ byteSwap64 (fromIntegral a :: Word64)
evalPrimOp BSwapOp    [WordV a] = WordV $ byteSwap64 a

-- Narrowings
evalPrimOp Narrow8IntOp   [IntV a]  = IntV  $ fromIntegral (fromIntegral a :: Int8)
evalPrimOp Narrow16IntOp  [IntV a]  = IntV  $ fromIntegral (fromIntegral a :: Int16)
evalPrimOp Narrow32IntOp  [IntV a]  = IntV  $ fromIntegral (fromIntegral a :: Int32)
evalPrimOp Narrow8WordOp  [WordV a] = WordV $ fromIntegral (fromIntegral a :: Word8)
evalPrimOp Narrow16WordOp [WordV a] = WordV $ fromIntegral (fromIntegral a :: Word16)
evalPrimOp Narrow32WordOp [WordV a] = WordV $ fromIntegral (fromIntegral a :: Word32)

evalPrimOp op args = error $ "unsupported op: " ++ show op ++ " with args: " ++ show args
