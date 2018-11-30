module Main (main) -- typecheck
where {
-- partain: import Fast2haskell;
-- #include "../Fast2haskell.hs"
--  hbcc prelude:
#ifdef __HBCC__
#include "prel.hs"
#endif
#ifdef __HBC__
import Char;
#endif
#ifdef __GLASGOW_HASKELL__
import Data.Char;
#endif
    strict_show_i::Int -> [Char];
    strict_show_i x = -- miraseq x (show x);
        let {
            x' = quot x 10;
            d = rem x 10;
            r' = chr (ord '0' + d)} in if x' == 0 then [r'] else strict_show_i x' ++ [r'];
    stringEQ :: String -> String -> Bool;
    stringEQ [] [] = True;
    stringEQ (x:xs) (y:ys) = x == y && stringEQ xs ys;
    stringEQ (x:xs) [] = False;
    stringEQ [] (y:ys) = False;
    f_check::T_vexp -> [Char];
    f_check a_expr=
        let {
            r_do_check=f_tc c_main_gamma c_main_ns a_expr;
            f_get_tpe (F_OK (a_phi,a_tpe))=a_tpe;
            f_failure_check C_FAILURE=True;
            f_failure_check a_x=False
         } in
            if (f_failure_check r_do_check)
            then "failure"
            else
                (f_tpr (f_get_tpe r_do_check));
    c_main_gamma::[(T_vname,T_type_scheme)];
    c_main_gamma=(:) ("add",F_SCHEME [] (f_arrow c_tint (f_arrow c_tint c_tint))) ((:)
        ("0",F_SCHEME [] c_tint) ((:) ("1",F_SCHEME [] c_tint) ((:) ("if",
        F_SCHEME ((:) "*" []) (f_arrow c_tbool (f_arrow c_star (f_arrow c_star c_star)))) ((:) ("cons",
        F_SCHEME ((:) "*" []) (f_arrow c_star (f_arrow (f_list c_star) (f_list c_star)))) ((:)
        ("head",F_SCHEME ((:) "*" []) (f_arrow (f_list c_star) c_star)) ((:) ("tail",
        F_SCHEME ((:) "*" []) (f_arrow (f_list c_star) (f_list c_star))) ((:) ("null",
        F_SCHEME ((:) "*" []) (f_arrow (f_list c_star) c_tbool)) ((:) ("NIL",F_SCHEME
        ((:) "*" []) (f_list c_star)) []))))))));
    c_star,c_starstar::T_type_exp;
    c_star=F_TVAR "*";
    c_starstar=F_TVAR "**";
    c_main_ns::T_name_supply;
    c_main_ns=(:) (0 :: Int) [];
    f_ap2::T_vexp -> T_vexp -> T_vexp;
    f_ap3::T_vexp -> T_vexp -> T_vexp -> T_vexp;
    f_ap4::T_vexp -> T_vexp -> T_vexp -> T_vexp -> T_vexp;
    f_ap2 a_a a_b=F_APE a_a a_b;
    f_ap3 a_a a_b a_c=F_APE (F_APE a_a a_b) a_c;
    f_ap4 a_a a_b a_c a_d=F_APE (F_APE (F_APE a_a a_b) a_c) a_d;
    c_prog_ones::(T_vname,T_vexp);
    c_rhs_ones::T_vexp;
    c_prog_ones=("ones",F_LETRECE ((:) "ones" []) ((:) c_rhs_ones []) c_var_ones);
    c_rhs_ones=f_ap3 c_cnst_cons c_cnst_one c_var_ones;
    c_prog_len::(T_vname,T_vexp);
    c_rhs_len,c_body_len::T_vexp;
    c_prog_len=("len ",F_LETRECE ((:) "len" []) ((:) c_rhs_len []) c_var_len);
    c_rhs_len=f_plambda ((:) "x" []) c_body_len;
    c_body_len=
        let {
            r_len_test=f_ap2 c_cnst_null c_var_x;
            r_len_tail=f_ap2 c_var_len (f_ap2 c_cnst_tail c_var_x)
         } in  f_ap4 c_cnst_if r_len_test c_cnst_zero (f_ap3 c_cnst_add c_cnst_one r_len_tail);
    c_prog_sum::(T_vname,T_vexp);
    c_rhs_sum,c_body_sum::T_vexp;
    c_prog_sum=("sum ",F_LETRECE ((:) "sum" []) ((:) c_rhs_sum []) c_var_sum);
    c_rhs_sum=f_plambda ((:) "x" []) c_body_sum;
    c_body_sum=
        let {
            r_sum_test=f_ap2 c_cnst_null c_var_x;
            r_sum_head=f_ap2 c_cnst_head c_var_x;
            r_sum_tail=f_ap2 c_var_sum (f_ap2 c_cnst_tail c_var_x)
         } in  f_ap4 c_cnst_if r_sum_test c_cnst_zero (f_ap3 c_cnst_add r_sum_head r_sum_tail);
    c_prog_map::(T_vname,T_vexp);
    c_rhs_map,c_body_map::T_vexp;
    c_prog_map=("map ",F_LETRECE ((:) "map" []) ((:) c_rhs_map []) c_var_map);
    c_rhs_map=f_plambda ((:) "f" ((:) "x" [])) c_body_map;
    c_body_map=
        let {
            r_map_test=f_ap2 c_cnst_null c_var_x;
            r_map_head=f_ap2 c_var_f (f_ap2 c_cnst_head c_var_x);
            r_map_tail=f_ap3 c_var_map c_var_f (f_ap2 c_cnst_tail c_var_x)
         } in  f_ap4 c_cnst_if r_map_test c_cnst_nil (f_ap3 c_cnst_cons r_map_head r_map_tail);
    c_prog_y::(T_vname,T_vexp);
    c_rhs_Y,c_body_Y::T_vexp;
    c_prog_y=("y   ",F_LETRECE ((:) "Y" []) ((:) c_rhs_Y []) c_var_Y);
    c_rhs_Y=f_plambda ((:) "f" []) c_body_Y;
    c_body_Y=f_ap2 c_var_f (f_ap2 c_var_Y c_var_f);
    c_prog_succ::(T_vname,T_vexp);
    c_body_succ::T_vexp;
    c_prog_succ=("succ",f_plambda ((:) "n" ((:) "a" ((:) "b" []))) c_body_succ);
    c_body_succ=f_ap3 c_var_b c_var_n (f_ap3 c_var_n c_var_a c_var_b);
    c_prog_skk::(T_vname,T_vexp);
    c_body_skk::T_vexp;
    c_prog_skk=("skk ",F_LETE ((:) "S" ((:) "K" [])) ((:) c_rhs_S ((:) c_rhs_K [])) c_body_skk);
    c_body_skk=f_ap3 c_var_S c_var_K c_var_K;
    c_rhs_S,c_body_S::T_vexp;
    c_rhs_S=f_plambda ((:) "x" ((:) "y" ((:) "z" []))) c_body_S;
    c_body_S=f_ap3 c_var_x c_var_z (f_ap2 c_var_y c_var_z);
    c_rhs_K::T_vexp;
    c_rhs_K=f_plambda ((:) "x" ((:) "y" [])) c_var_x;
    c_prog_inc::(T_vname,T_vexp);
    c_prog_inc=("inc ",f_plambda ((:) "x" []) c_body_inc);
    c_body_inc=f_ap3 c_cnst_add c_cnst_one c_var_x;
    c_cnst_add,c_cnst_cons,c_cnst_head,c_cnst_if,c_cnst_nil,c_cnst_null,c_cnst_one,c_cnst_tail,c_cnst_zero::T_vexp;
    c_cnst_add=F_VAR "add";
    c_cnst_cons=F_VAR "cons";
    c_cnst_head=F_VAR "head";
    c_cnst_if=F_VAR "if";
    c_cnst_nil=F_VAR "NIL";
    c_cnst_null=F_VAR "null";
    c_cnst_one=F_VAR "1";
    c_cnst_tail=F_VAR "tail";
    c_cnst_zero=F_VAR "0";
    c_var_K,c_var_S,c_var_Y,c_var_a,c_var_b,c_var_f,c_var_len,c_var_map,c_var_n,c_var_ones,c_var_sum,c_var_x,c_var_y,c_var_z::T_vexp;
    c_var_K=F_VAR "K";
    c_var_S=F_VAR "S";
    c_var_Y=F_VAR "Y";
    c_var_a=F_VAR "a";
    c_var_b=F_VAR "b";
    c_var_f=F_VAR "f";
    c_var_len=F_VAR "len";
    c_var_map=F_VAR "map";
    c_var_n=F_VAR "n";
    c_var_ones=F_VAR "ones";
    c_var_sum=F_VAR "sum";
    c_var_x=F_VAR "x";
    c_var_y=F_VAR "y";
    c_var_z=F_VAR "z";
    f_plambda::[T_vname] -> T_vexp -> T_vexp;
    f_plambda a_vs a_e=foldr F_LAMBDA a_e a_vs;
type
    T_vname=[Char];
data
    T_vexp=F_VAR T_vname | F_LAMBDA T_vname T_vexp | F_APE T_vexp T_vexp | F_LETE [T_vname] [T_vexp] T_vexp | F_LETRECE [T_vname] [T_vexp] T_vexp;
type
    T_tvname=[Char];
type
    T_tcname=[Char];
data
    T_type_exp=F_TVAR T_tvname | F_TCONS T_tcname [T_type_exp];
    f_vname_equal::T_vname -> T_vname -> Bool;
    f_vname_equal a_vx a_vy=(stringEQ :: ([Char] -> [Char] -> Bool)) a_vx a_vy;
    f_tvname_equal::T_tvname -> T_tvname -> Bool;
    f_tvname_equal a_tvx a_tvy=(stringEQ :: ([Char] -> [Char] -> Bool)) a_tvx a_tvy;
    f_tcname_equal::T_tcname -> T_tcname -> Bool;
    f_tcname_equal a_tcx a_tcy=(stringEQ :: ([Char] -> [Char] -> Bool)) a_tcx a_tcy;
    f_type_exp_equal::T_type_exp -> T_type_exp -> Bool;
    f_type_exp_equal (F_TVAR a_tvx) (F_TVAR a_tvy)=f_tvname_equal a_tvx a_tvy;
    f_type_exp_equal (F_TCONS a_tcx a_txs) (F_TCONS a_tcy a_tys)=
        if (f_tcname_equal a_tcx a_tcy)
        then (f_type_exp_list_equal a_txs a_tys)
        else
            False;
    f_type_exp_equal a_x a_y=False;
    f_type_exp_list_equal::[T_type_exp] -> [T_type_exp] -> Bool;
    f_type_exp_list_equal [] []=True;
    f_type_exp_list_equal [] a_tys=False;
    f_type_exp_list_equal a_txs []=False;
    f_type_exp_list_equal (a_tx:a_txs) (a_ty:a_tys)=
        if (f_type_exp_equal a_tx a_ty)
        then (f_type_exp_list_equal a_txs a_tys)
        else
            False;
    f_tpr::T_type_exp -> [Char];
    f_tpr (F_TVAR a_tvname)=(++) "<" ((++) a_tvname ">");
    f_tpr (F_TCONS x (a_t1:a_t2:[]))| x `stringEQ` "arrow" =(++) "(" ((++) (f_tpr a_t1) ((++) " -> " ((++) (f_tpr a_t2) ")")));
    f_tpr (F_TCONS x (a_t:[])) | x `stringEQ` "list" =(++) "[" ((++) (f_tpr a_t) "]");
    f_tpr (F_TCONS x (a_t1:a_t2:[]))| x `stringEQ` "cross" =(++) "(" ((++) (f_tpr a_t1) ((++) " x " ((++) (f_tpr a_t2) ")")));
    f_tpr (F_TCONS a_other [])=a_other;
    f_tpr (F_TCONS a_other a_ts)=(++) "(" ((++) a_other ((++) (concat (map ((.)
        ((:) ' ') f_tpr) a_ts)) ")"));
    f_arrow::T_type_exp -> T_type_exp -> T_type_exp;
    f_arrow a_t1 a_t2=F_TCONS "arrow" ((:) a_t1 ((:) a_t2 []));
    c_tint,c_tbool::T_type_exp;
    c_tint=F_TCONS "int" [];
    c_tbool=F_TCONS "bool" [];
    f_cross::T_type_exp -> T_type_exp -> T_type_exp;
    f_cross a_t1 a_t2=F_TCONS "cross" ((:) a_t1 ((:) a_t2 []));
    f_list::T_type_exp -> T_type_exp;
    f_list a_t=F_TCONS "list" ((:) a_t []);
    f_tvars_in::T_type_exp -> [T_tvname];
    f_tvars_in a_t=
        let {
            f_tvars_in' (F_TVAR a_x) a_l=(:) a_x a_l;
            f_tvars_in' (F_TCONS a_y a_ts) a_l=foldr f_tvars_in' a_l a_ts
         } in  f_tvars_in' a_t [];
data
    T_reply t1=F_OK t1 | C_FAILURE;
type
    T_subst=T_tvname -> T_type_exp;
    f_sub_type::T_subst -> T_type_exp -> T_type_exp;
    f_sub_type a_phi (F_TVAR a_tvn)=a_phi a_tvn;
    f_sub_type a_phi (F_TCONS a_tcn a_ts)=F_TCONS a_tcn (map (f_sub_type a_phi) a_ts);
    f_scomp::T_subst -> T_subst -> T_subst;
    f_scomp a_sub2 a_sub1 a_tvn=f_sub_type a_sub2 (a_sub1 a_tvn);
    f_id_subst::T_subst;
    f_id_subst a_tvn=F_TVAR a_tvn;
    f_delta::T_tvname -> T_type_exp -> T_subst;
    f_delta a_tvn a_t a_tvn'=
        if (f_tvname_equal a_tvn a_tvn')
        then a_t
        else
            (F_TVAR a_tvn');
    f_extend::T_subst -> T_tvname -> T_type_exp -> T_reply T_subst;
    f_extend a_phi a_tvn a_t=
        if (f_type_exp_equal a_t (F_TVAR a_tvn))
        then (F_OK a_phi)
        else
        if (f_in a_tvn (f_tvars_in a_t))
        then C_FAILURE
        else
            (F_OK (f_scomp (f_delta a_tvn a_t) a_phi));
    f_unify::T_subst -> (T_type_exp,T_type_exp) -> T_reply T_subst;
    f_unify a_phi ((F_TVAR a_tvn),a_t)=
        let {
            r_phitvn=a_phi a_tvn;
            r_phit=f_sub_type a_phi a_t
         } in
            if (f_type_exp_equal r_phitvn (F_TVAR a_tvn))
            then (f_extend a_phi a_tvn r_phit)
            else
                (f_unify a_phi (r_phitvn,r_phit));
    f_unify a_phi ((F_TCONS a_tcn a_ts),(F_TVAR a_tvn))=f_unify a_phi (F_TVAR a_tvn,F_TCONS a_tcn a_ts);
    f_unify a_phi ((F_TCONS a_tcn a_ts),(F_TCONS a_tcn' a_ts'))=
        if (f_tcname_equal a_tcn a_tcn')
        then (f_unifyl a_phi (f_zip2 a_ts a_ts'))
        else
            C_FAILURE;
    f_unifyl::T_subst -> [(T_type_exp,T_type_exp)] -> T_reply T_subst;
    f_unifyl a_phi a_eqns=
        let {
            f_unify' a_eqn (F_OK a_phi)=f_unify a_phi a_eqn;
            f_unify' a_eqn C_FAILURE=C_FAILURE
         } in  foldr f_unify' (F_OK a_phi) a_eqns;
data
    T_type_scheme=F_SCHEME [T_tvname] T_type_exp;
    f_unknowns_scheme::T_type_scheme -> [T_tvname];
    f_unknowns_scheme (F_SCHEME a_scvs a_t)=f_bar (f_tvars_in a_t) a_scvs;
    f_bar::[T_tvname] -> [T_tvname] -> [T_tvname];
    f_bar a_xs a_ys=[a_x|a_x<-a_xs,not (f_in a_x a_ys)];
    f_in::T_tvname -> [T_tvname] -> Bool;
    f_in a_x' []=False;
    f_in a_x' (a_x:a_xs)=
        if (f_tvname_equal a_x a_x')
        then True
        else
            (f_in a_x' a_xs);
    f_sub_scheme::T_subst -> T_type_scheme -> T_type_scheme;
    f_sub_scheme a_phi (F_SCHEME a_scvs a_t)=F_SCHEME a_scvs (f_sub_type (f_exclude a_phi a_scvs) a_t);
    f_exclude::T_subst -> [T_tvname] -> T_tvname -> T_type_exp;
    f_exclude a_phi a_scvs a_tvn=
        if (f_in a_tvn a_scvs)
        then (F_TVAR a_tvn)
        else
            (a_phi a_tvn);
type
    T_assoc_list t1 t2=[(t1,t2)];
type
    T_type_env=T_assoc_list T_vname T_type_scheme;
    f_dom::T_type_env -> [T_vname];
    f_dom a_al=[a_k|(a_k,a_v)<-a_al];
    f_val::T_type_env -> T_vname -> T_type_scheme;
    f_val a_al a_k=head [a_v|(a_k',a_v)<-a_al,f_vname_equal a_k a_k'];
    f_install::T_type_env -> T_vname -> T_type_scheme -> T_type_env;
    f_install a_al a_k a_v=(:) (a_k,a_v) a_al;
    f_rng::T_type_env -> [T_type_scheme];
    f_rng a_al=map (f_val a_al) (f_dom a_al);
    f_unknowns_te::T_type_env -> [T_tvname];
    f_unknowns_te a_gamma=concat (map f_unknowns_scheme (f_rng a_gamma));
    f_sub_te::T_subst -> T_type_env -> T_type_env;
    f_sub_te a_phi a_gamma=[(a_x,f_sub_scheme a_phi a_st)|(a_x,a_st)<-a_gamma];
type
    T_name_supply=[Int];
    f_next_name::T_name_supply -> T_tvname;
    f_next_name a_ns=tail (concat (map ((.) ((:) '.') strict_show_i) a_ns));
    f_deplete::T_name_supply -> T_name_supply;
    f_deplete (a_n:a_ns)=(:) (((+) :: (Int -> Int -> Int)) a_n (2 :: Int)) a_ns;
    f_split::T_name_supply -> (T_name_supply,T_name_supply);
    f_split a_ns=((:) (0 :: Int) a_ns,(:) (1 :: Int) a_ns);
    f_name_sequence::T_name_supply -> [T_tvname];
    f_name_sequence a_ns=(:) (f_next_name a_ns) (f_name_sequence (f_deplete a_ns));
    f_tc::T_type_env -> T_name_supply -> T_vexp -> T_reply (T_subst,T_type_exp);
    f_tc a_gamma a_ns (F_VAR a_x)=f_tcvar a_gamma a_ns a_x;
    f_tc a_gamma a_ns (F_APE a_e1 a_e2)=f_tcap a_gamma a_ns a_e1 a_e2;
    f_tc a_gamma a_ns (F_LAMBDA a_x a_e)=f_tclambda a_gamma a_ns a_x a_e;
    f_tc a_gamma a_ns (F_LETE a_xs a_es a_e)=f_tclet a_gamma a_ns a_xs a_es a_e;
    f_tc a_gamma a_ns (F_LETRECE a_xs a_es a_e)=f_tcletrec a_gamma a_ns a_xs a_es a_e;
    f_tcl::T_type_env -> T_name_supply -> [T_vexp] -> T_reply (T_subst,[T_type_exp]);
    f_tcl a_gamma a_ns []=F_OK (f_id_subst,[]);
    f_tcl a_gamma a_ns (a_e:a_es)=
        let {
            (r_ns0,r_ns1)=f_split a_ns
         } in  f_tcl1 a_gamma r_ns0 a_es (f_tc a_gamma r_ns1 a_e);
    f_tcl1::T_type_env -> T_name_supply -> [T_vexp] -> (T_reply (T_subst,T_type_exp)) -> T_reply (T_subst,[T_type_exp]);
    f_tcl1 a_gamma a_ns a_es C_FAILURE=C_FAILURE;
    f_tcl1 a_gamma a_ns a_es (F_OK (a_phi,a_t))=
        let {
            r_gamma'=f_sub_te a_phi a_gamma
         } in  f_tcl2 a_phi a_t (f_tcl r_gamma' a_ns a_es);
    f_tcl2::T_subst -> T_type_exp -> (T_reply (T_subst,[T_type_exp])) -> T_reply (T_subst,[T_type_exp]);
    f_tcl2 a_phi a_t C_FAILURE=C_FAILURE;
    f_tcl2 a_phi a_t (F_OK (a_psi,a_ts))=F_OK (f_scomp a_psi a_phi,(:) (f_sub_type a_psi a_t) a_ts);
    f_tcvar::T_type_env -> T_name_supply -> T_vname -> T_reply (T_subst,T_type_exp);
    f_tcvar a_gamma a_ns a_x=
        let {
            r_scheme=f_val a_gamma a_x
         } in  F_OK (f_id_subst,f_newinstance a_ns r_scheme);
    f_newinstance::T_name_supply -> T_type_scheme -> T_type_exp;
    f_newinstance a_ns (F_SCHEME a_scvs a_t)=
        let {
            r_al=f_zip2 a_scvs (f_name_sequence a_ns);
            r_phi=f_al_to_subst r_al
         } in  f_sub_type r_phi a_t;
    f_al_to_subst::(T_assoc_list T_tvname T_tvname) -> T_subst;
    f_al_to_subst a_al a_tvn=
        if (f_in a_tvn (f_dom' a_al))
        then (F_TVAR (f_val' a_al a_tvn))
        else
            (F_TVAR a_tvn);
    f_dom'::(T_assoc_list T_tvname T_tvname) -> [T_vname];
    f_dom' a_al=[a_k|(a_k,a_v)<-a_al];
    f_val'::(T_assoc_list T_tvname T_tvname) -> T_tvname -> T_tvname;
    f_val' a_al a_k=head [a_v|(a_k',a_v)<-a_al,f_tvname_equal a_k a_k'];
    f_tcap::T_type_env -> T_name_supply -> T_vexp -> T_vexp -> T_reply (T_subst,T_type_exp);
    f_tcap a_gamma a_ns a_e1 a_e2=
        let {
            r_tvn=f_next_name a_ns;
            r_ns'=f_deplete a_ns
         } in  f_tcap1 r_tvn (f_tcl a_gamma r_ns' ((:) a_e1 ((:) a_e2 [])));
    f_tcap1::T_tvname -> (T_reply (T_subst,[T_type_exp])) -> T_reply (T_subst,T_type_exp);
    f_tcap1 a_tvn C_FAILURE=C_FAILURE;
    f_tcap1 a_tvn (F_OK (a_phi,(a_t1:a_t2:[])))=f_tcap2 a_tvn (f_unify a_phi (a_t1,f_arrow a_t2 (F_TVAR a_tvn)));
    f_tcap2::T_tvname -> (T_reply T_subst) -> T_reply (T_subst,T_type_exp);
    f_tcap2 a_tvn C_FAILURE=C_FAILURE;
    f_tcap2 a_tvn (F_OK a_phi)=F_OK (a_phi,a_phi a_tvn);
    f_tclambda::T_type_env -> T_name_supply -> T_vname -> T_vexp -> T_reply (T_subst,T_type_exp);
    f_tclambda a_gamma a_ns a_x a_e=
        let {
            r_ns'=f_deplete a_ns;
            r_gamma'=(:) (f_new_bvar (a_x,r_tvn)) a_gamma;
            r_tvn=f_next_name a_ns
         } in  f_tclambda1 r_tvn (f_tc r_gamma' r_ns' a_e);
    f_tclambda1::T_tvname -> (T_reply (T_subst,T_type_exp)) -> T_reply (T_subst,T_type_exp);
    f_tclambda1 a_tvn C_FAILURE=C_FAILURE;
    f_tclambda1 a_tvn (F_OK (a_phi,a_t))=F_OK (a_phi,f_arrow (a_phi a_tvn) a_t);
    f_new_bvar::(T_vname,T_tvname) -> (T_vname,T_type_scheme);
    f_new_bvar (a_x,a_tvn)=(a_x,F_SCHEME [] (F_TVAR a_tvn));
    f_tclet::T_type_env -> T_name_supply -> [T_vname] -> [T_vexp] -> T_vexp -> T_reply (T_subst,T_type_exp);
    f_tclet a_gamma a_ns a_xs a_es a_e=
        let {
            (r_ns0,r_ns1)=f_split a_ns
         } in  f_tclet1 a_gamma r_ns0 a_xs a_e (f_tcl a_gamma r_ns1 a_es);
    f_tclet1::T_type_env -> T_name_supply -> [T_vname] -> T_vexp -> (T_reply (T_subst,[T_type_exp])) -> T_reply (T_subst,T_type_exp);
    f_tclet1 a_gamma a_ns a_xs a_e C_FAILURE=C_FAILURE;
    f_tclet1 a_gamma a_ns a_xs a_e (F_OK (a_phi,a_ts))=
        let {
            r_gamma''=f_add_decls r_gamma' r_ns0 a_xs a_ts;
            r_gamma'=f_sub_te a_phi a_gamma;
            (r_ns0,r_ns1)=f_split a_ns
         } in  f_tclet2 a_phi (f_tc r_gamma'' r_ns1 a_e);
    f_tclet2::T_subst -> (T_reply (T_subst,T_type_exp)) -> T_reply (T_subst,T_type_exp);
    f_tclet2 a_phi C_FAILURE=C_FAILURE;
    f_tclet2 a_phi (F_OK (a_phi',a_t))=F_OK (f_scomp a_phi' a_phi,a_t);
    f_add_decls a_gamma a_ns a_xs a_ts=
        let {
            r_schemes=map (f_genbar r_unknowns a_ns) a_ts;
            r_unknowns=f_unknowns_te a_gamma
         } in  (++) (f_zip2 a_xs r_schemes) a_gamma;
    f_genbar::[T_tvname] -> T_name_supply -> T_type_exp -> T_type_scheme;
    f_genbar a_unknowns a_ns a_t=
        let {
            r_al=f_zip2 r_scvs (f_name_sequence a_ns);
            r_scvs=f_bar (f_nodups (f_tvars_in a_t)) a_unknowns;
            r_t'=f_sub_type (f_al_to_subst r_al) a_t
         } in  F_SCHEME (map f_snd r_al) r_t';
    f_nodups::[T_tvname] -> [T_tvname];
    f_nodups a_xs=
        let {
            f_f a_acc []=a_acc;
            f_f a_acc (a_x:a_xs)=
                if (f_in a_x a_acc)
                then (f_f a_acc a_xs)
                else
                    (f_f ((:) a_x a_acc) a_xs)
         } in  f_f [] a_xs;
    f_tcletrec::T_type_env -> T_name_supply -> [T_tvname] -> [T_vexp] -> T_vexp -> T_reply (T_subst,T_type_exp);
    f_tcletrec a_gamma a_ns a_xs a_es a_e=
        let {
            (r_ns0,r_ns')=f_split a_ns;
            (r_ns1,r_ns2)=f_split r_ns';
            r_nbvs=f_new_bvars a_xs r_ns2
         } in  f_tcletrec1 a_gamma r_ns0 r_nbvs a_e (f_tcl ((++) r_nbvs a_gamma) r_ns1 a_es);
    f_new_bvars::[T_tvname] -> T_name_supply -> T_type_env;
    f_new_bvars a_xs a_ns=map f_new_bvar (f_zip2 a_xs (f_name_sequence a_ns));
    f_tcletrec1::T_type_env -> T_name_supply -> T_type_env -> T_vexp -> (T_reply (T_subst,[T_type_exp])) -> T_reply (T_subst,T_type_exp);
    f_tcletrec1 a_gamma a_ns a_nbvs a_e C_FAILURE=C_FAILURE;
    f_tcletrec1 a_gamma a_ns a_nbvs a_e (F_OK (a_phi,a_ts))=
        let {
            r_ts'=map f_old_bvar r_nbvs';
            r_nbvs'=f_sub_te a_phi a_nbvs;
            r_gamma'=f_sub_te a_phi a_gamma
         } in  f_tcletrec2 r_gamma' a_ns r_nbvs' a_e (f_unifyl a_phi (f_zip2 a_ts r_ts'));
    f_old_bvar::(T_tvname,T_type_scheme) -> T_type_exp;
    f_old_bvar (a_x,(F_SCHEME [] a_t))=a_t;
    f_tcletrec2::T_type_env -> T_name_supply -> T_type_env -> T_vexp -> (T_reply T_subst) -> T_reply (T_subst,T_type_exp);
    f_tcletrec2 a_gamma a_ns a_nbvs a_e C_FAILURE=C_FAILURE;
    f_tcletrec2 a_gamma a_ns a_nbvs a_e (F_OK a_phi)=
        let {
            r_ts=map f_old_bvar r_nbvs';
            r_nbvs'=f_sub_te a_phi a_nbvs;
            r_gamma'=f_sub_te a_phi a_gamma;
            r_gamma''=f_add_decls r_gamma' r_ns0 (map f_fst a_nbvs) r_ts;
            (r_ns0,r_ns1)=f_split a_ns
         } in  f_tclet2 a_phi (f_tc r_gamma'' r_ns1 a_e);
    f_benchmark_main::Int -> [Char];
    f_benchmark_main a_n=
        let {
            f_do_check (a_name,a_expr)=(++) a_name ((++) " :: " ((++) (f_check a_expr) "\n"))
         } in  (++) (f_sumcode (concat (map f_do_check (concat (f_take a_n
            (f_repeat c_progs)))))) "\n";
    c_progs::[(T_tvname,T_vexp)];
    c_progs=(:) c_prog_ones ((:) c_prog_len ((:) c_prog_map ((:) c_prog_skk ((:) c_prog_inc ((:) c_prog_sum
        ((:) c_prog_y ((:) c_prog_succ [])))))));
    f_sumcode::[Char] -> [Char];
    f_sumcode a_xs=
        let {
            f_sumcode' [] a_sum a_n=(++) (strict_show_i (((+) :: (Int -> Int -> Int)) a_sum a_n)) ((:) '/' (strict_show_i a_n));
            f_sumcode' (a_x:a_xs) a_sum a_n=f_sumcode' a_xs (((+) :: (Int -> Int -> Int)) a_sum (fromEnum a_x)) (((+) :: (Int -> Int -> Int)) a_n (1 :: Int))
         } in  f_sumcode' a_xs (0 :: Int) (0 :: Int);
    f_fst::(t1,t2) -> t1;
    f_fst (a_a,a_b)=a_a;
    f_repeat::t1 -> [t1];
    f_repeat a_x=(:) a_x (f_repeat a_x);
    f_snd::(t1,t2) -> t2;
    f_snd (a_a,a_b)=a_b;
    f_take::Int -> [t1] -> [t1];
    f_take 0 a_x=[];
    f_take a_n (a_a:a_x)=(:) a_a (f_take (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x);
    f_take a_n a_x=[];
    f_zip2::[t1] -> [t2] -> [(t1,t2)];
    f_zip2 (a_a:a_x) (a_b:a_y)=(:) (a_a,a_b) (f_zip2 a_x a_y);
    f_zip2 a_x a_y=[];
    f_main a_x=f_benchmark_main a_x;
--    c_input=(600 :: Int);
    c_input=(200 :: Int);
    main = putStr (f_main c_input)
}
