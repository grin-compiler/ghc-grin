{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module Parser (parse) where

import Lexer (lex_tok)
import ParserM (Token(..), ParserM, run_parser, get_pos, show_pos,
                happyError)
import Syntax
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (Info) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (Info)
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ([Option]) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([Option])
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ([Option]) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ([Option])
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (Option) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (Option)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (Maybe Fixity) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (Maybe Fixity)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ([Entry]) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ([Entry])
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Entry) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Entry)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (Entry) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (Entry)
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Entry) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Entry)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Entry) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Entry)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (Entry) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (Entry)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ([Option]) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ([Option])
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Category) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Category)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (String) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (String)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (String) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (String)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (String) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (String)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (String) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (String)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([(String, String, Int)]) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ([(String, String, Int)])
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ([(String, String, Int)]) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ([(String, String, Int)])
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ((String, String, Int)) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ((String, String, Int))
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Ty) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Ty)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Ty) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Ty)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Ty) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Ty)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ([Ty]) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ([Ty])
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: ([Ty]) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> ([Ty])
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Ty) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Ty)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (TyCon) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (TyCon)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x91\x00\x91\x00\x5a\x00\xea\xff\x89\x00\x00\x00\xea\xff\x9c\x00\x9b\x00\x95\x00\x8a\x00\x5a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x86\x00\x7e\x00\x7a\x00\x02\x00\x94\x00\x7d\x00\x00\x00\x08\x00\xfc\xff\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x79\x00\x93\x00\x00\x00\x00\x00\x23\x00\x90\x00\x49\x00\x00\x00\x00\x00\x72\x00\x71\x00\x6e\x00\x00\x00\x00\x00\x87\x00\x00\x00\xfd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x8e\x00\x8f\x00\x8d\x00\x8c\x00\x00\x00\x00\x00\x08\x00\x00\x00\x8b\x00\x00\x00\x02\x00\x02\x00\x7b\x00\x00\x00\xea\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x7b\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x83\x00\xfd\xff\xfd\xff\x00\x00\x84\x00\x88\x00\x64\x00\x00\x00\x00\x00\x00\x00\x85\x00\x82\x00\x00\x00\x81\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x73\x00\x00\x00\x00\x00\x61\x00\x80\x00\x00\x00\x5b\x00\x77\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x7c\x00\x0d\x00\x56\x00\x78\x00\x00\x00\x00\x00\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x69\x00\x00\x00\x00\x00\x55\x00\x33\x00\x1b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x67\x00\x00\x00\x00\x00\x74\x00\x70\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\xff\x63\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x39\x00\x00\x00\x00\x00\x00\x00\x29\x00\x26\x00\x75\x00\x00\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\x41\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xef\xff\xfb\xff\x00\x00\xfd\xff\xfb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xef\xff\xee\xff\xed\xff\xec\xff\xeb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdf\xff\xd2\xff\xd0\xff\xc9\xff\x00\x00\x00\x00\xc4\xff\xc3\xff\xc2\xff\xce\xff\xc6\xff\x00\x00\x00\x00\xdf\xff\xf0\xff\xfe\xff\x00\x00\x00\x00\x00\x00\xfc\xff\xf5\xff\x00\x00\x00\x00\x00\x00\xf1\xff\xf6\xff\xd6\xff\xf8\xff\xdc\xff\xf9\xff\xfa\xff\xf7\xff\xe7\xff\xe0\xff\x00\x00\xdf\xff\xcb\xff\x00\x00\x00\x00\xc5\xff\xd1\xff\xc9\xff\xc7\xff\x00\x00\xc8\xff\x00\x00\x00\x00\xe5\xff\xe9\xff\xfb\xff\xd3\xff\xd4\xff\xca\xff\xcf\xff\xcd\xff\x00\x00\xe5\xff\x00\x00\xe4\xff\xe3\xff\xe2\xff\xe1\xff\x00\x00\xdc\xff\xdc\xff\xda\xff\x00\x00\xd7\xff\x00\x00\xf2\xff\xf3\xff\xf4\xff\x00\x00\xd6\xff\xd9\xff\x00\x00\xdd\xff\xde\xff\xdf\xff\xe8\xff\xcc\xff\xe6\xff\xe5\xff\xdb\xff\xd8\xff\x00\x00\x00\x00\xea\xff\x00\x00\x00\x00\xd5\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x05\x00\x06\x00\x07\x00\x0c\x00\x1b\x00\x09\x00\x05\x00\x04\x00\x07\x00\x20\x00\x12\x00\x13\x00\x05\x00\x01\x00\x25\x00\x0d\x00\x0e\x00\x14\x00\x15\x00\x16\x00\x17\x00\x0f\x00\x10\x00\x1a\x00\x0b\x00\x14\x00\x15\x00\x16\x00\x21\x00\x22\x00\x23\x00\x1a\x00\x25\x00\x26\x00\x21\x00\x22\x00\x23\x00\x29\x00\x25\x00\x26\x00\x21\x00\x22\x00\x23\x00\x09\x00\x25\x00\x26\x00\x14\x00\x15\x00\x16\x00\x17\x00\x0f\x00\x10\x00\x1a\x00\x02\x00\x03\x00\x15\x00\x16\x00\x14\x00\x15\x00\x16\x00\x14\x00\x15\x00\x16\x00\x1a\x00\x0f\x00\x10\x00\x1a\x00\x14\x00\x15\x00\x16\x00\x14\x00\x15\x00\x16\x00\x1a\x00\x28\x00\x0b\x00\x1a\x00\x14\x00\x15\x00\x16\x00\x18\x00\x19\x00\x1a\x00\x1a\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x18\x00\x19\x00\x1a\x00\x0d\x00\x0e\x00\x12\x00\x13\x00\x0d\x00\x0e\x00\x0d\x00\x0e\x00\x02\x00\x03\x00\x02\x00\x03\x00\x00\x00\x01\x00\x01\x00\x02\x00\x0b\x00\x11\x00\x0e\x00\x28\x00\x04\x00\x0e\x00\x13\x00\x26\x00\x09\x00\x04\x00\x26\x00\x0a\x00\x04\x00\x0a\x00\x13\x00\x0d\x00\x0c\x00\x06\x00\x06\x00\x04\x00\x0d\x00\x08\x00\x28\x00\x09\x00\x03\x00\x28\x00\x28\x00\x0b\x00\x09\x00\x09\x00\x03\x00\x03\x00\x27\x00\x27\x00\xff\xff\xff\xff\x26\x00\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\x24\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x19\x00\x3c\x00\x1a\x00\x4d\x00\x08\x00\x55\x00\x19\x00\x28\x00\x1a\x00\x09\x00\x68\x00\x57\x00\x40\x00\x02\x00\x0a\x00\x66\x00\x35\x00\x38\x00\x15\x00\x16\x00\x64\x00\x5f\x00\x53\x00\x17\x00\x6b\x00\x62\x00\x15\x00\x16\x00\x1b\x00\x1c\x00\x1d\x00\x17\x00\x1e\x00\x1f\x00\x1b\x00\x1c\x00\x1d\x00\x56\x00\x1e\x00\x1f\x00\x1b\x00\x1c\x00\x1d\x00\x31\x00\x41\x00\x1f\x00\x38\x00\x15\x00\x16\x00\x39\x00\x60\x00\x53\x00\x17\x00\x65\x00\x06\x00\x32\x00\x33\x00\x46\x00\x15\x00\x16\x00\x47\x00\x15\x00\x16\x00\x17\x00\x52\x00\x53\x00\x17\x00\x37\x00\x15\x00\x16\x00\x3a\x00\x15\x00\x16\x00\x17\x00\x34\x00\x63\x00\x17\x00\x14\x00\x15\x00\x16\x00\x48\x00\x3d\x00\x3e\x00\x17\x00\x22\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x11\x00\x12\x00\x13\x00\x14\x00\x3c\x00\x3d\x00\x3e\x00\x4c\x00\x35\x00\x56\x00\x57\x00\x34\x00\x35\x00\x43\x00\x35\x00\x27\x00\x06\x00\x05\x00\x06\x00\x04\x00\x02\x00\x42\x00\x43\x00\x44\x00\x2d\x00\x2f\x00\x6e\x00\x6d\x00\x6f\x00\x46\x00\x6b\x00\x31\x00\x6a\x00\x5d\x00\x68\x00\x5e\x00\x62\x00\x46\x00\x59\x00\x5f\x00\x3c\x00\x4a\x00\x4c\x00\x59\x00\x4b\x00\x5a\x00\x31\x00\x25\x00\x5b\x00\x5c\x00\x2f\x00\x31\x00\x31\x00\x26\x00\x27\x00\x37\x00\x20\x00\x00\x00\x00\x00\x21\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 61) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61)
	]

happy_n_terms = 43 :: Int
happy_n_nonterms = 27 :: Int

happyReduce_1 = happySpecReduce_3  0# happyReduction_1
happyReduction_1 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (Info happy_var_1 happy_var_2
	)}}

happyReduce_2 = happySpecReduce_2  1# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (happy_var_2
	)}

happyReduce_3 = happySpecReduce_2  2# happyReduction_3
happyReduction_3 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_4 = happySpecReduce_0  2# happyReduction_4
happyReduction_4  =  happyIn6
		 ([]
	)

happyReduce_5 = happySpecReduce_3  3# happyReduction_5
happyReduction_5 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TLowerName happy_var_1) -> 
	happyIn7
		 (OptionFalse  happy_var_1
	)}

happyReduce_6 = happySpecReduce_3  3# happyReduction_6
happyReduction_6 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TLowerName happy_var_1) -> 
	happyIn7
		 (OptionTrue   happy_var_1
	)}

happyReduce_7 = happySpecReduce_3  3# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TLowerName happy_var_1) -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (OptionString happy_var_1 happy_var_3
	)}}

happyReduce_8 = happySpecReduce_3  3# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TLowerName happy_var_1) -> 
	case happyOutTok happy_x_3 of { (TInteger happy_var_3) -> 
	happyIn7
		 (OptionInteger happy_var_1 happy_var_3
	)}}

happyReduce_9 = happySpecReduce_3  3# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (OptionVector happy_var_3
	)}

happyReduce_10 = happySpecReduce_3  3# happyReduction_10
happyReduction_10 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn7
		 (OptionFixity happy_var_3
	)}

happyReduce_11 = happySpecReduce_2  4# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TInteger happy_var_2) -> 
	happyIn8
		 (Just $ Fixity NoSourceText happy_var_2 InfixN
	)}

happyReduce_12 = happySpecReduce_2  4# happyReduction_12
happyReduction_12 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TInteger happy_var_2) -> 
	happyIn8
		 (Just $ Fixity NoSourceText happy_var_2 InfixL
	)}

happyReduce_13 = happySpecReduce_2  4# happyReduction_13
happyReduction_13 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TInteger happy_var_2) -> 
	happyIn8
		 (Just $ Fixity NoSourceText happy_var_2 InfixR
	)}

happyReduce_14 = happySpecReduce_1  4# happyReduction_14
happyReduction_14 happy_x_1
	 =  happyIn8
		 (Nothing
	)

happyReduce_15 = happySpecReduce_2  5# happyReduction_15
happyReduction_15 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_16 = happySpecReduce_0  5# happyReduction_16
happyReduction_16  =  happyIn9
		 ([]
	)

happyReduce_17 = happySpecReduce_1  6# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_18 = happySpecReduce_1  6# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_19 = happySpecReduce_1  6# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_20 = happySpecReduce_1  6# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_21 = happyReduce 7# 7# happyReduction_21
happyReduction_21 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TUpperName happy_var_2) -> 
	case happyOutTok happy_x_3 of { (TString happy_var_3) -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	case happyOut24 happy_x_5 of { happy_var_5 -> 
	case happyOut17 happy_x_6 of { happy_var_6 -> 
	case happyOut15 happy_x_7 of { happy_var_7 -> 
	happyIn11
		 (PrimOpSpec {
                    cons = happy_var_2,
                    name = happy_var_3,
                    cat = happy_var_4,
                    ty = happy_var_5,
                    desc = happy_var_6,
                    opts = happy_var_7
                }
	) `HappyStk` happyRest}}}}}}

happyReduce_22 = happyReduce 4# 8# happyReduction_22
happyReduction_22 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut15 happy_x_4 of { happy_var_4 -> 
	happyIn12
		 (PrimTypeSpec { ty = happy_var_2, desc = happy_var_3, opts = happy_var_4 }
	) `HappyStk` happyRest}}}

happyReduce_23 = happyReduce 5# 9# happyReduction_23
happyReduction_23 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TString happy_var_2) -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	case happyOut17 happy_x_4 of { happy_var_4 -> 
	case happyOut15 happy_x_5 of { happy_var_5 -> 
	happyIn13
		 (PseudoOpSpec { name = happy_var_2, ty = happy_var_3, desc = happy_var_4, opts = happy_var_5 }
	) `HappyStk` happyRest}}}}

happyReduce_24 = happySpecReduce_3  10# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TString happy_var_2) -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (Section { title = happy_var_2, desc = happy_var_3 }
	)}}

happyReduce_25 = happySpecReduce_2  11# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (happy_var_2
	)}

happyReduce_26 = happySpecReduce_0  11# happyReduction_26
happyReduction_26  =  happyIn15
		 ([]
	)

happyReduce_27 = happySpecReduce_1  12# happyReduction_27
happyReduction_27 happy_x_1
	 =  happyIn16
		 (Dyadic
	)

happyReduce_28 = happySpecReduce_1  12# happyReduction_28
happyReduction_28 happy_x_1
	 =  happyIn16
		 (Monadic
	)

happyReduce_29 = happySpecReduce_1  12# happyReduction_29
happyReduction_29 happy_x_1
	 =  happyIn16
		 (Compare
	)

happyReduce_30 = happySpecReduce_1  12# happyReduction_30
happyReduction_30 happy_x_1
	 =  happyIn16
		 (GenPrimOp
	)

happyReduce_31 = happySpecReduce_1  13# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (happy_var_1
	)}

happyReduce_32 = happySpecReduce_0  13# happyReduction_32
happyReduction_32  =  happyIn17
		 (""
	)

happyReduce_33 = happySpecReduce_3  14# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (happy_var_2
	)}

happyReduce_34 = happySpecReduce_2  15# happyReduction_34
happyReduction_34 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (happy_var_1 ++ happy_var_2
	)}}

happyReduce_35 = happySpecReduce_0  15# happyReduction_35
happyReduction_35  =  happyIn19
		 (""
	)

happyReduce_36 = happySpecReduce_3  16# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 ("{" ++ happy_var_2 ++ "}"
	)}

happyReduce_37 = happySpecReduce_1  16# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TNoBraces happy_var_1) -> 
	happyIn20
		 (happy_var_1
	)}

happyReduce_38 = happySpecReduce_3  17# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn21
		 (happy_var_2
	)}

happyReduce_39 = happySpecReduce_3  18# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 ([happy_var_1] ++ happy_var_3
	)}}

happyReduce_40 = happySpecReduce_1  18# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ([happy_var_1]
	)}

happyReduce_41 = happySpecReduce_0  18# happyReduction_41
happyReduction_41  =  happyIn22
		 ([]
	)

happyReduce_42 = happyReduce 7# 19# happyReduction_42
happyReduction_42 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TUpperName happy_var_2) -> 
	case happyOutTok happy_x_4 of { (TUpperName happy_var_4) -> 
	case happyOutTok happy_x_6 of { (TInteger happy_var_6) -> 
	happyIn23
		 ((happy_var_2, happy_var_4, happy_var_6)
	) `HappyStk` happyRest}}}

happyReduce_43 = happySpecReduce_3  20# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (TyF happy_var_1 happy_var_3
	)}}

happyReduce_44 = happySpecReduce_3  20# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (TyC happy_var_1 happy_var_3
	)}}

happyReduce_45 = happySpecReduce_1  20# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (happy_var_1
	)}

happyReduce_46 = happySpecReduce_2  21# happyReduction_46
happyReduction_46 happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (TyApp happy_var_1 happy_var_2
	)}}

happyReduce_47 = happySpecReduce_1  21# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (happy_var_1
	)}

happyReduce_48 = happySpecReduce_3  21# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (happy_var_2
	)}

happyReduce_49 = happySpecReduce_1  21# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TLowerName happy_var_1) -> 
	happyIn25
		 (TyVar happy_var_1
	)}

happyReduce_50 = happySpecReduce_3  22# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn26
		 (TyUTup happy_var_2
	)}

happyReduce_51 = happySpecReduce_3  23# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_52 = happySpecReduce_1  23# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 ([happy_var_1]
	)}

happyReduce_53 = happySpecReduce_2  24# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_54 = happySpecReduce_0  24# happyReduction_54
happyReduction_54  =  happyIn28
		 ([]
	)

happyReduce_55 = happySpecReduce_1  25# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TLowerName happy_var_1) -> 
	happyIn29
		 (TyVar happy_var_1
	)}

happyReduce_56 = happySpecReduce_1  25# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (TyApp happy_var_1 []
	)}

happyReduce_57 = happySpecReduce_1  26# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TUpperName happy_var_1) -> 
	happyIn30
		 (TyCon happy_var_1
	)}

happyReduce_58 = happySpecReduce_2  26# happyReduction_58
happyReduction_58 happy_x_2
	happy_x_1
	 =  happyIn30
		 (TyCon "()"
	)

happyReduce_59 = happySpecReduce_1  26# happyReduction_59
happyReduction_59 happy_x_1
	 =  happyIn30
		 (SCALAR
	)

happyReduce_60 = happySpecReduce_1  26# happyReduction_60
happyReduction_60 happy_x_1
	 =  happyIn30
		 (VECTOR
	)

happyReduce_61 = happySpecReduce_1  26# happyReduction_61
happyReduction_61 happy_x_1
	 =  happyIn30
		 (VECTUPLE
	)

happyNewToken action sts stk
	= lex_tok(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	TEOF -> happyDoAction 42# tk action sts stk;
	TArrow -> cont 1#;
	TDArrow -> cont 2#;
	TEquals -> cont 3#;
	TComma -> cont 4#;
	TOpenParen -> cont 5#;
	TCloseParen -> cont 6#;
	TOpenParenHash -> cont 7#;
	THashCloseParen -> cont 8#;
	TOpenBrace -> cont 9#;
	TCloseBrace -> cont 10#;
	TOpenBracket -> cont 11#;
	TCloseBracket -> cont 12#;
	TOpenAngle -> cont 13#;
	TCloseAngle -> cont 14#;
	TSection -> cont 15#;
	TPrimop -> cont 16#;
	TPseudoop -> cont 17#;
	TPrimtype -> cont 18#;
	TWith -> cont 19#;
	TDefaults -> cont 20#;
	TTrue -> cont 21#;
	TFalse -> cont 22#;
	TDyadic -> cont 23#;
	TMonadic -> cont 24#;
	TCompare -> cont 25#;
	TGenPrimOp -> cont 26#;
	TFixity -> cont 27#;
	TInfixN -> cont 28#;
	TInfixL -> cont 29#;
	TInfixR -> cont 30#;
	TNothing -> cont 31#;
	TVector -> cont 32#;
	TSCALAR -> cont 33#;
	TVECTOR -> cont 34#;
	TVECTUPLE -> cont 35#;
	TThatsAllFolks -> cont 36#;
	TLowerName happy_dollar_dollar -> cont 37#;
	TUpperName happy_dollar_dollar -> cont 38#;
	TString happy_dollar_dollar -> cont 39#;
	TInteger happy_dollar_dollar -> cont 40#;
	TNoBraces happy_dollar_dollar -> cont 41#;
	_ -> happyError' tk
	})

happyError_ 42# tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => ParserM a -> (a -> ParserM b) -> ParserM b
happyThen = (>>=)
happyReturn :: () => a -> ParserM a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> ParserM a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> ParserM a
happyError' tk = (\token -> happyError) tk

parsex = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDoSeq


parse :: String -> Either String Info
parse = run_parser parsex
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4













































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/opt/exp/ghc/roots/8.2.1/lib/ghc-8.2.1/include/ghcversion.h" #-}















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc23530_0/ghc_2.h" #-}




































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
