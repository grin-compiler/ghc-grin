-- $Id: event.hs,v 1.8 1999/02/10 01:26:47 boquist Exp $
module Main (main) -- event
where {
-- hbcc prelude:
#ifdef __HBCC__
#include "prel.hs"
#endif
pair          :: [x] -> Bool;
pair          []                  = False;
pair          x                   = True;
upto :: Int -> Int -> [Int];
upto m n = if m > n then [] else m : upto (m+1) n;
data 
    T_threestate=C_X | C_L | C_H;
    f_showstate::T_threestate -> [Char];
    f_showstate C_H="H";
    f_showstate C_L="L";
    f_showstate a_x="x";
    f_nandfun::T_threestate -> T_threestate -> T_threestate;
    f_nandfun C_H a_y=f_nandfunh a_y;
    f_nandfun C_L a_y=f_nandfunl a_y;
    f_nandfun C_X a_y=f_nandfunx a_y;
    f_nandfunh::T_threestate -> T_threestate;
    f_nandfunh C_H=C_L;
    f_nandfunh C_L=C_H;
    f_nandfunh C_X=C_X;
    f_nandfunl::T_threestate -> T_threestate;
    f_nandfunl C_H=C_H;
    f_nandfunl C_L=C_H;
    f_nandfunl C_X=C_H;
    f_nandfunx::T_threestate -> T_threestate;
    f_nandfunx C_H=C_X;
    f_nandfunx C_L=C_H;
    f_nandfunx C_X=C_X;
    f_threestate_cmp::T_threestate -> T_threestate -> Int;
    f_threestate_cmp C_H a_y=f_threestate_cmph a_y;
    f_threestate_cmp C_L a_y=f_threestate_cmpl a_y; 
    f_threestate_cmp C_X a_y=f_threestate_cmpx a_y;

    f_threestate_cmph::T_threestate -> Int;
    f_threestate_cmph C_H =(0 :: Int);
    f_threestate_cmph C_L =(1 :: Int);
    f_threestate_cmph C_X =(1 :: Int);

    f_threestate_cmpl::T_threestate -> Int;
    f_threestate_cmpl C_H =(1 :: Int);
    f_threestate_cmpl C_L =(0 :: Int);
    f_threestate_cmpl C_X =(1 :: Int);

    f_threestate_cmpx::T_threestate -> Int;
    f_threestate_cmpx C_H =(1 :: Int);
    f_threestate_cmpx C_L =(1 :: Int);
    f_threestate_cmpx C_X =(0 :: Int);
type 
    T_comb2=Int -> Int -> Int -> Int -> T_state -> T_event;
data 
    T_func2=C_NoFn | F_Fn T_comb2 Int Int;
data 
    T_dep=F_Pak Int T_func2;
data 
    T_event=F_At Int Int T_threestate;
type 
    T_state=[T_threestate];
    f_event_cmp::T_event -> T_event -> Int;
    f_event_cmp (F_At a_atime a_awire a_awhat) (F_At a_btime a_bwire a_bwhat)=
        if (((<) :: (Int -> Int -> Bool)) a_atime a_btime)
        then c_cmp_less
        else 
        if (((>) :: (Int -> Int -> Bool)) a_atime a_btime)
        then c_cmp_greater
        else 
            c_cmp_equal;
    f_update1::T_state -> Int -> T_threestate -> T_state;
    f_update1 a_st a_i a_val=(++) (f_take a_i a_st) ((:) a_val (f_drop (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) a_st));
    f_simulate::[T_event] -> T_state -> Int -> [T_event];
    f_simulate [] a_st a_et=[];
    f_simulate ((F_At a_time a_wire a_what):a_es) a_st a_et=
        let { 
            r_e=F_At a_time a_wire a_what;
            r_newes=f_merge_cmp f_event_cmp a_es (f_sort_cmp f_event_cmp r_more);
            r_newst=f_update1 a_st a_wire a_what;
            r_more=[f_mkevent a_out|a_out<-f_dependencies a_wire];
            f_mkevent a_wire=f_recalculate (f_d a_wire) a_time a_wire r_newst
         } in  
            if (((>=) :: (Int -> Int -> Bool)) a_time a_et)
            then []
            else 
            if (((==) :: (Int -> Int -> Bool)) (f_threestate_cmp (a_st !! a_wire) a_what) (0 :: Int))
            then (f_simulate a_es a_st a_et)
            else 
                ((:) r_e (f_simulate r_newes r_newst a_et));
    f_nand::T_comb2;
    f_nand a_x a_y a_time a_wire a_st=F_At (((+) :: (Int -> Int -> Int)) a_time (3 :: Int)) a_wire (f_nandfun (a_st !! a_x) (a_st !! a_y));
    f_clock::Int -> Int -> Int -> Int -> [T_event];
    f_clock a_wire a_t a_low a_high=(:) (F_At a_t a_wire C_L) ((:) (F_At (((+) :: (Int -> Int -> Int)) a_t a_low) a_wire C_H) (f_clock a_wire 
        (((+) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) a_t a_low) a_high) a_low a_high));
    f_findlaststate::[T_event] -> [T_threestate] -> [T_threestate];
    f_findlaststate [] a_st=a_st;
    f_findlaststate ((F_At a_t a_wire a_what):a_es) a_st=f_findlaststate a_es (f_update1 a_st a_wire a_what);
    f_recalculate::T_func2 -> Int -> Int -> [T_threestate] -> T_event;
    f_recalculate (F_Fn a_f a_x a_y)=a_f a_x a_y;
    c_getdep::[[Int]];
    c_getdep=foldl f_inserte [[]|a_i<-(upto (1 :: Int) c_nwires)] [F_Pak a_x (f_d a_x)|a_x<-(upto (0 :: Int) (((-) :: (Int -> Int -> Int)) c_nwires (1 :: Int)))];
    f_update2::[[Int]] -> Int -> [Int] -> [[Int]];
    f_update2 a_st a_i a_val=(++) (f_take a_i a_st) ((:) a_val (f_drop (((+) :: (Int -> Int -> Int)) a_i (1 :: Int)) a_st));
    f_inserte::[[Int]] -> T_dep -> [[Int]];
    f_inserte a_x (F_Pak a_y C_NoFn)=a_x;
    f_inserte a_x (F_Pak a_y (F_Fn a_a a_b a_c))=f_update2 (f_update2 a_x a_b (f_unify (a_x !! a_b) a_y)) a_c (f_unify (a_x !! a_c) a_y);
    f_unify::[Int] -> Int -> [Int];
    f_unify [] a_n=(:) a_n [];
    f_unify (a_a:a_b) a_n=
        if (((==) :: (Int -> Int -> Bool)) a_a a_n)
        then ((:) a_a a_b)
        else 
            ((:) a_a (f_unify a_b a_n));
    f_dependencies::Int -> [Int];
    f_dependencies a_i=c_getdep !! a_i;
    c_istate::[T_threestate];
    c_istate=[C_X|a_i<-(upto (1 :: Int) c_nwires)];
    c_nwires=(13 :: Int);
    c_ievents::[T_event];
    c_ievents=f_merge_cmp f_event_cmp (f_clock (1 :: Int) (0 :: Int) (50 :: Int) (50 :: Int)) (f_clock (0 :: Int) (25 :: Int) (100 :: Int) (100 :: Int));
    f_d::Int -> T_func2;
    f_d 3=F_Fn f_nand (0 :: Int) (0 :: Int);
    f_d 4=F_Fn f_nand (0 :: Int) (1 :: Int);
    f_d 5=F_Fn f_nand (3 :: Int) (1 :: Int);
    f_d 6=F_Fn f_nand (4 :: Int) (7 :: Int);
    f_d 7=F_Fn f_nand (5 :: Int) (6 :: Int);
    f_d 8=F_Fn f_nand (1 :: Int) (1 :: Int);
    f_d 9=F_Fn f_nand (8 :: Int) (6 :: Int);
    f_d 10=F_Fn f_nand (8 :: Int) (7 :: Int);
    f_d 11=F_Fn f_nand (9 :: Int) (12 :: Int);
    f_d 12=F_Fn f_nand (10 :: Int) (11 :: Int);
    f_d a_x=C_NoFn;
    f_benchmark_main a_x=(++) (concat (map f_showstate (f_findlaststate (f_simulate c_ievents c_istate a_x) c_istate))) "\n";
    c_cmp_less::Int;
    c_cmp_less=((negate) :: (Int -> Int)) (1 :: Int);
    c_cmp_equal::Int;
    c_cmp_equal=(0 :: Int);
    c_cmp_greater::Int;
    c_cmp_greater=(1 :: Int);
    f_cmp_i::Int -> Int -> Int;
    f_cmp_i a_x a_y=((-) :: (Int -> Int -> Int)) a_x a_y;
    f_merge_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1] -> [t1];
    f_merge_cmp a_cmp [] a_y=a_y;
    f_merge_cmp a_cmp (a_a:a_x) []=(:) a_a a_x;
    f_merge_cmp a_cmp (a_a:a_x) (a_b:a_y)=
        if (((<=) :: (Int -> Int -> Bool)) (a_cmp a_a a_b) c_cmp_equal)
        then ((:) a_a (f_merge_cmp a_cmp a_x ((:) a_b a_y)))
        else 
            ((:) a_b (f_merge_cmp a_cmp ((:) a_a a_x) a_y));
    f_sort_cmp::(t1 -> t1 -> Int) -> [t1] -> [t1];
    f_sort_cmp a_cmp a_x=
        let { 
            r_n=length a_x;
            r_n2=((quot) :: (Int -> Int -> Int)) r_n (2 :: Int)
         } in  
            if (((<=) :: (Int -> Int -> Bool)) r_n (1 :: Int))
            then a_x
            else 
                (f_merge_cmp a_cmp (f_sort_cmp a_cmp (f_take r_n2 a_x)) (f_sort_cmp a_cmp (f_drop r_n2 a_x)));
    f_drop::Int -> [t1] -> [t1];
    f_drop 0 a_x=a_x;
    f_drop a_n (a_a:a_x)=f_drop (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x;
    f_drop a_n a_x=[];
    f_take::Int -> [t1] -> [t1];
    f_take 0 a_x=[];
    f_take a_n (a_a:a_x)=(:) a_a (f_take (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x);
    f_take a_n a_x=[];
    f_main a_x=f_benchmark_main a_x;
--    c_input=(400000 :: Int);
    c_input=(150000 :: Int);
    main = putStr (f_main c_input)
}
