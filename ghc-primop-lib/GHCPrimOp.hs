module GHCPrimOp where

import Data.Int
import Data.Word

-- 64 bit platform semantics
type PrimWord = Word64
type PrimInt  = Int64

data Value
  = CharV   Word32  -- HINT: utf32 encoding
  | IntV    PrimInt
  | WordV   PrimWord
  | FloatV  Float   -- 32 bit floating point
  | DoubleV Double  -- 64 bit floating point
  | TupleV  [Value] -- HINT: tuples can not contain tuples, just simple values i.e. int, word, char
  deriving (Eq, Ord, Show)

data PrimOp
  -- Char#
  = CharGtOp
  | CharGeOp
  | CharEqOp
  | CharNeOp
  | CharLtOp
  | CharLeOp
  | OrdOp
  -- Int#
  | IntAddOp
  | IntSubOp
  | IntMulOp
  | IntMulMayOfloOp
  | IntQuotOp
  | IntRemOp
  | IntQuotRemOp
  | AndIOp
  | OrIOp
  | XorIOp
  | NotIOp
  | IntNegOp
  | IntAddCOp
  | IntSubCOp
  | IntGtOp
  | IntGeOp
  | IntEqOp
  | IntNeOp
  | IntLtOp
  | IntLeOp
  | ChrOp
  | Int2WordOp
  | Int2FloatOp
  | Int2DoubleOp
  | Word2FloatOp
  | Word2DoubleOp
  | ISllOp
  | ISraOp
  | ISrlOp
  -- Word#
  | WordAddOp
  | WordAddCOp
  | WordSubCOp
  | WordAdd2Op
  | WordSubOp
  | WordMulOp
  | WordMul2Op
  | WordQuotOp
  | WordRemOp
  | WordQuotRemOp
  | WordQuotRem2Op
  | AndOp
  | OrOp
  | XorOp
  | NotOp
  | SllOp
  | SrlOp
  | Word2IntOp
  | WordGtOp
  | WordGeOp
  | WordEqOp
  | WordNeOp
  | WordLtOp
  | WordLeOp
  | PopCnt8Op
  | PopCnt16Op
  | PopCnt32Op
  | PopCnt64Op
  | PopCntOp
  | Pdep8Op   -- TODO
  | Pdep16Op  -- TODO
  | Pdep32Op  -- TODO
  | Pdep64Op  -- TODO
  | PdepOp    -- TODO
  | Pext8Op   -- TODO
  | Pext16Op  -- TODO
  | Pext32Op  -- TODO
  | Pext64Op  -- TODO
  | PextOp    -- TODO
  | Clz8Op
  | Clz16Op
  | Clz32Op
  | Clz64Op
  | ClzOp
  | Ctz8Op
  | Ctz16Op
  | Ctz32Op
  | Ctz64Op
  | CtzOp
  | BSwap16Op
  | BSwap32Op
  | BSwap64Op
  | BSwapOp
  -- Narrowings
  | Narrow8IntOp
  | Narrow16IntOp
  | Narrow32IntOp
  | Narrow8WordOp
  | Narrow16WordOp
  | Narrow32WordOp
  -- Double#
  | DoubleGtOp
  | DoubleGeOp
  | DoubleEqOp
  | DoubleNeOp
  | DoubleLtOp
  | DoubleLeOp
  | DoubleAddOp
  | DoubleSubOp
  | DoubleMulOp
  | DoubleDivOp
  | DoubleNegOp
  | DoubleFabsOp
  | Double2IntOp
  | Double2FloatOp
  | DoubleExpOp
  | DoubleLogOp
  | DoubleSqrtOp
  | DoubleSinOp
  | DoubleCosOp
  | DoubleTanOp
  | DoubleAsinOp
  | DoubleAcosOp
  | DoubleAtanOp
  | DoubleSinhOp
  | DoubleCoshOp
  | DoubleTanhOp
  | DoublePowerOp
  | DoubleDecode_2IntOp   -- TODO
  | DoubleDecode_Int64Op  -- TODO
  -- Float#
  | FloatGtOp
  | FloatGeOp
  | FloatEqOp
  | FloatNeOp
  | FloatLtOp
  | FloatLeOp
  | FloatAddOp
  | FloatSubOp
  | FloatMulOp
  | FloatDivOp
  | FloatNegOp
  | FloatFabsOp
  | Float2IntOp
  | FloatExpOp
  | FloatLogOp
  | FloatSqrtOp
  | FloatSinOp
  | FloatCosOp
  | FloatTanOp
  | FloatAsinOp
  | FloatAcosOp
  | FloatAtanOp
  | FloatSinhOp
  | FloatCoshOp
  | FloatTanhOp
  | FloatPowerOp
  | Float2DoubleOp
  | FloatDecode_IntOp -- TODO
----------------------------
  -- Arrays
  | NewArrayOp
  | SameMutableArrayOp
  | ReadArrayOp
  | WriteArrayOp
  | SizeofArrayOp
  | SizeofMutableArrayOp
  | IndexArrayOp
  | UnsafeFreezeArrayOp
  | UnsafeThawArrayOp
  | CopyArrayOp
  | CopyMutableArrayOp
  | CloneArrayOp
  | CloneMutableArrayOp
  | FreezeArrayOp
  | ThawArrayOp
  | CasArrayOp
  -- Small Arrays
  | NewSmallArrayOp
  | SameSmallMutableArrayOp
  | ReadSmallArrayOp
  | WriteSmallArrayOp
  | SizeofSmallArrayOp
  | SizeofSmallMutableArrayOp
  | IndexSmallArrayOp
  | UnsafeFreezeSmallArrayOp
  | UnsafeThawSmallArrayOp
  | CopySmallArrayOp
  | CopySmallMutableArrayOp
  | CloneSmallArrayOp
  | CloneSmallMutableArrayOp
  | FreezeSmallArrayOp
  | ThawSmallArrayOp
  | CasSmallArrayOp
  -- Byte Arrays
  | NewByteArrayOp_Char
  | NewPinnedByteArrayOp_Char
  | NewAlignedPinnedByteArrayOp_Char
  | MutableByteArrayIsPinnedOp
  | ByteArrayIsPinnedOp
  | ByteArrayContents_Char
  | SameMutableByteArrayOp
  | ShrinkMutableByteArrayOp_Char
  | ResizeMutableByteArrayOp_Char
  | UnsafeFreezeByteArrayOp
  | SizeofByteArrayOp
  | SizeofMutableByteArrayOp
  | GetSizeofMutableByteArrayOp
  | IndexByteArrayOp_Char
  | IndexByteArrayOp_WideChar
  | IndexByteArrayOp_Int
  | IndexByteArrayOp_Word
  | IndexByteArrayOp_Addr
  | IndexByteArrayOp_Float
  | IndexByteArrayOp_Double
  | IndexByteArrayOp_StablePtr
  | IndexByteArrayOp_Int8
  | IndexByteArrayOp_Int16
  | IndexByteArrayOp_Int32
  | IndexByteArrayOp_Int64
  | IndexByteArrayOp_Word8
  | IndexByteArrayOp_Word16
  | IndexByteArrayOp_Word32
  | IndexByteArrayOp_Word64
  | IndexByteArrayOp_Word8AsChar
  | IndexByteArrayOp_Word8AsWideChar
  | IndexByteArrayOp_Word8AsAddr
  | IndexByteArrayOp_Word8AsFloat
  | IndexByteArrayOp_Word8AsDouble
  | IndexByteArrayOp_Word8AsStablePtr
  | IndexByteArrayOp_Word8AsInt16
  | IndexByteArrayOp_Word8AsInt32
  | IndexByteArrayOp_Word8AsInt64
  | IndexByteArrayOp_Word8AsInt
  | IndexByteArrayOp_Word8AsWord16
  | IndexByteArrayOp_Word8AsWord32
  | IndexByteArrayOp_Word8AsWord64
  | IndexByteArrayOp_Word8AsWord
  | ReadByteArrayOp_Char
  | ReadByteArrayOp_WideChar
  | ReadByteArrayOp_Int
  | ReadByteArrayOp_Word
  | ReadByteArrayOp_Addr
  | ReadByteArrayOp_Float
  | ReadByteArrayOp_Double
  | ReadByteArrayOp_StablePtr
  | ReadByteArrayOp_Int8
  | ReadByteArrayOp_Int16
  | ReadByteArrayOp_Int32
  | ReadByteArrayOp_Int64
  | ReadByteArrayOp_Word8
  | ReadByteArrayOp_Word16
  | ReadByteArrayOp_Word32
  | ReadByteArrayOp_Word64
  | ReadByteArrayOp_Word8AsChar
  | ReadByteArrayOp_Word8AsWideChar
  | ReadByteArrayOp_Word8AsAddr
  | ReadByteArrayOp_Word8AsFloat
  | ReadByteArrayOp_Word8AsDouble
  | ReadByteArrayOp_Word8AsStablePtr
  | ReadByteArrayOp_Word8AsInt16
  | ReadByteArrayOp_Word8AsInt32
  | ReadByteArrayOp_Word8AsInt64
  | ReadByteArrayOp_Word8AsInt
  | ReadByteArrayOp_Word8AsWord16
  | ReadByteArrayOp_Word8AsWord32
  | ReadByteArrayOp_Word8AsWord64
  | ReadByteArrayOp_Word8AsWord
  | WriteByteArrayOp_Char
  | WriteByteArrayOp_WideChar
  | WriteByteArrayOp_Int
  | WriteByteArrayOp_Word
  | WriteByteArrayOp_Addr
  | WriteByteArrayOp_Float
  | WriteByteArrayOp_Double
  | WriteByteArrayOp_StablePtr
  | WriteByteArrayOp_Int8
  | WriteByteArrayOp_Int16
  | WriteByteArrayOp_Int32
  | WriteByteArrayOp_Int64
  | WriteByteArrayOp_Word8
  | WriteByteArrayOp_Word16
  | WriteByteArrayOp_Word32
  | WriteByteArrayOp_Word64
  | WriteByteArrayOp_Word8AsChar
  | WriteByteArrayOp_Word8AsWideChar
  | WriteByteArrayOp_Word8AsAddr
  | WriteByteArrayOp_Word8AsFloat
  | WriteByteArrayOp_Word8AsDouble
  | WriteByteArrayOp_Word8AsStablePtr
  | WriteByteArrayOp_Word8AsInt16
  | WriteByteArrayOp_Word8AsInt32
  | WriteByteArrayOp_Word8AsInt64
  | WriteByteArrayOp_Word8AsInt
  | WriteByteArrayOp_Word8AsWord16
  | WriteByteArrayOp_Word8AsWord32
  | WriteByteArrayOp_Word8AsWord64
  | WriteByteArrayOp_Word8AsWord
  | CompareByteArraysOp
  | CopyByteArrayOp
  | CopyMutableByteArrayOp
  | CopyByteArrayToAddrOp
  | CopyMutableByteArrayToAddrOp
  | CopyAddrToByteArrayOp
  | SetByteArrayOp
  | AtomicReadByteArrayOp_Int
  | AtomicWriteByteArrayOp_Int
  | CasByteArrayOp_Int
  | FetchAddByteArrayOp_Int
  | FetchSubByteArrayOp_Int
  | FetchAndByteArrayOp_Int
  | FetchNandByteArrayOp_Int
  | FetchOrByteArrayOp_Int
  | FetchXorByteArrayOp_Int
  -- Arrays of arrays
  | NewArrayArrayOp
  | SameMutableArrayArrayOp
  | UnsafeFreezeArrayArrayOp
  | SizeofArrayArrayOp
  | SizeofMutableArrayArrayOp
  | IndexArrayArrayOp_ByteArray
  | IndexArrayArrayOp_ArrayArray
  | ReadArrayArrayOp_ByteArray
  | ReadArrayArrayOp_MutableByteArray
  | ReadArrayArrayOp_ArrayArray
  | ReadArrayArrayOp_MutableArrayArray
  | WriteArrayArrayOp_ByteArray
  | WriteArrayArrayOp_MutableByteArray
  | WriteArrayArrayOp_ArrayArray
  | WriteArrayArrayOp_MutableArrayArray
  | CopyArrayArrayOp
  | CopyMutableArrayArrayOp
  -- Addr#
  | AddrAddOp
  | AddrSubOp
  | AddrRemOp
  | Addr2IntOp
  | Int2AddrOp
  | AddrGtOp
  | AddrGeOp
  | AddrEqOp
  | AddrNeOp
  | AddrLtOp
  | AddrLeOp
  | IndexOffAddrOp_Char
  | IndexOffAddrOp_WideChar
  | IndexOffAddrOp_Int
  | IndexOffAddrOp_Word
  | IndexOffAddrOp_Addr
  | IndexOffAddrOp_Float
  | IndexOffAddrOp_Double
  | IndexOffAddrOp_StablePtr
  | IndexOffAddrOp_Int8
  | IndexOffAddrOp_Int16
  | IndexOffAddrOp_Int32
  | IndexOffAddrOp_Int64
  | IndexOffAddrOp_Word8
  | IndexOffAddrOp_Word16
  | IndexOffAddrOp_Word32
  | IndexOffAddrOp_Word64
  | ReadOffAddrOp_Char
  | ReadOffAddrOp_WideChar
  | ReadOffAddrOp_Int
  | ReadOffAddrOp_Word
  | ReadOffAddrOp_Addr
  | ReadOffAddrOp_Float
  | ReadOffAddrOp_Double
  | ReadOffAddrOp_StablePtr
  | ReadOffAddrOp_Int8
  | ReadOffAddrOp_Int16
  | ReadOffAddrOp_Int32
  | ReadOffAddrOp_Int64
  | ReadOffAddrOp_Word8
  | ReadOffAddrOp_Word16
  | ReadOffAddrOp_Word32
  | ReadOffAddrOp_Word64
  | WriteOffAddrOp_Char
  | WriteOffAddrOp_WideChar
  | WriteOffAddrOp_Int
  | WriteOffAddrOp_Word
  | WriteOffAddrOp_Addr
  | WriteOffAddrOp_Float
  | WriteOffAddrOp_Double
  | WriteOffAddrOp_StablePtr
  | WriteOffAddrOp_Int8
  | WriteOffAddrOp_Int16
  | WriteOffAddrOp_Int32
  | WriteOffAddrOp_Int64
  | WriteOffAddrOp_Word8
  | WriteOffAddrOp_Word16
  | WriteOffAddrOp_Word32
  | WriteOffAddrOp_Word64
  -- Mutable variables
  | NewMutVarOp
  | ReadMutVarOp
  | WriteMutVarOp
  | SameMutVarOp
  | AtomicModifyMutVarOp
  | CasMutVarOp
  -- Exceptions
  | CatchOp
  | RaiseOp
  | RaiseIOOp
  | MaskAsyncExceptionsOp
  | MaskUninterruptibleOp
  | UnmaskAsyncExceptionsOp
  | MaskStatus
  -- STM-accessible Mutable Variables
  | AtomicallyOp
  | RetryOp
  | CatchRetryOp
  | CatchSTMOp
  | NewTVarOp
  | ReadTVarOp
  | ReadTVarIOOp
  | WriteTVarOp
  | SameTVarOp
  -- Synchronized Mutable Variables
  | NewMVarOp
  | TakeMVarOp
  | TryTakeMVarOp
  | PutMVarOp
  | TryPutMVarOp
  | ReadMVarOp
  | TryReadMVarOp
  | SameMVarOp
  | IsEmptyMVarOp
  -- Delay/wait operations
  | DelayOp
  | WaitReadOp
  | WaitWriteOp
  -- Concurrency primitives
  | ForkOp
  | ForkOnOp
  | KillThreadOp
  | YieldOp
  | MyThreadIdOp
  | LabelThreadOp
  | IsCurrentThreadBoundOp
  | NoDuplicateOp
  | ThreadStatusOp
  -- Weak pointers
  | MkWeakOp
  | MkWeakNoFinalizerOp
  | AddCFinalizerToWeakOp
  | DeRefWeakOp
  | FinalizeWeakOp
  | TouchOp
  -- Stable pointers and names
  | MakeStablePtrOp
  | DeRefStablePtrOp
  | EqStablePtrOp
  | MakeStableNameOp
  | EqStableNameOp
  | StableNameToIntOp
  -- Compact normal form
  | CompactNewOp
  | CompactResizeOp
  | CompactContainsOp
  | CompactContainsAnyOp
  | CompactGetFirstBlockOp
  | CompactGetNextBlockOp
  | CompactAllocateBlockOp
  | CompactFixupPointersOp
  | CompactAdd
  | CompactAddWithSharing
  | CompactSize
  -- Unsafe pointer equality
  | ReallyUnsafePtrEqualityOp
  -- Parallelism
  | ParOp
  | SparkOp
  | SeqOp
  | GetSparkOp
  | NumSparks
  -- Tag to enum stuff
  | DataToTagOp
  | TagToEnumOp
  -- Bytecode operations
  | AddrToAnyOp
  | AnyToAddrOp
  | MkApUpd0_Op
  | NewBCOOp
  | UnpackClosureOp
  | GetApStackValOp
  -- Misc
  | GetCCSOfOp
  | GetCurrentCCSOp
  | ClearCCSOp
  -- Etc
  | TraceEventOp
  | TraceMarkerOp
  | GetThreadAllocationCounter
  | SetThreadAllocationCounter
  -- SIMD Vectors
{-
  | VecBroadcastOp PrimOpVecCat Length Width
  | VecPackOp PrimOpVecCat Length Width
  | VecUnpackOp PrimOpVecCat Length Width
  | VecInsertOp PrimOpVecCat Length Width
  | VecAddOp PrimOpVecCat Length Width
  | VecSubOp PrimOpVecCat Length Width
  | VecMulOp PrimOpVecCat Length Width
  | VecDivOp PrimOpVecCat Length Width
  | VecQuotOp PrimOpVecCat Length Width
  | VecRemOp PrimOpVecCat Length Width
  | VecNegOp PrimOpVecCat Length Width
  | VecIndexByteArrayOp PrimOpVecCat Length Width
  | VecReadByteArrayOp PrimOpVecCat Length Width
  | VecWriteByteArrayOp PrimOpVecCat Length Width
  | VecIndexOffAddrOp PrimOpVecCat Length Width
  | VecReadOffAddrOp PrimOpVecCat Length Width
  | VecWriteOffAddrOp PrimOpVecCat Length Width
  | VecIndexScalarByteArrayOp PrimOpVecCat Length Width
  | VecReadScalarByteArrayOp PrimOpVecCat Length Width
  | VecWriteScalarByteArrayOp PrimOpVecCat Length Width
  | VecIndexScalarOffAddrOp PrimOpVecCat Length Width
  | VecReadScalarOffAddrOp PrimOpVecCat Length Width
  | VecWriteScalarOffAddrOp PrimOpVecCat Length Width
-}
  -- Prefetch
  | PrefetchByteArrayOp3
  | PrefetchMutableByteArrayOp3
  | PrefetchAddrOp3
  | PrefetchValueOp3
  | PrefetchByteArrayOp2
  | PrefetchMutableByteArrayOp2
  | PrefetchAddrOp2
  | PrefetchValueOp2
  | PrefetchByteArrayOp1
  | PrefetchMutableByteArrayOp1
  | PrefetchAddrOp1
  | PrefetchValueOp1
  | PrefetchByteArrayOp0
  | PrefetchMutableByteArrayOp0
  | PrefetchAddrOp0
  | PrefetchValueOp0
  deriving (Eq, Ord, Show)
