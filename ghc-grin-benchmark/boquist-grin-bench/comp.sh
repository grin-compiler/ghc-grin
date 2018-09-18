# $Id: comp.sh,v 1.51 1999/05/06 14:41:36 boquist Exp $

add () {
    SUBDIRS="$SUBDIRS $@"
}

delete () {
    local i tmp=""
    for i in $SUBDIRS; do
	if [ "$i" != "$1" ]; then
	    tmp="$tmp $i"
	fi
    done
    SUBDIRS="$tmp"
}

subdirs_other () {
    add other/nfib
    add other/tsumupto
if [ "$HOSTNAME" != padda ]; then
    add other/sieve
    add other/queens
    add other/words
    add other/puzzle
fi
}

subdirs_imaginary () {
    add imaginary/tak
if [ "$HOSTNAME" != padda ]; then
    add imaginary/exp3_8
fi
}

subdirs_spectral () {
    add spectral/awards
if [ "$HOSTNAME" != padda ]; then
    add spectral/sorting
    add spectral/cichelli
fi
if [ "$HOSTNAME" != padda ]; then
    add spectral/hartel/event
    #add spectral/hartel/sched
    add spectral/clausify
    add spectral/hartel/ida
if [ "$HOSTNAME" != blixten ]; then
    add spectral/hartel/typecheck
fi
    #add spectral/rewrite
fi
    add spectral/boyer2
}

subdirs () {
    SUBDIRS=
    subdirs_other
    subdirs_imaginary
    subdirs_spectral
}

# set default:
subdirs

#------------------------------------------------------------------------------

recurse () {
    for d in $SUBDIRS; do
	echo -n "SUBDIR ======> $d"
	if [ -n "$OUT" ]; then echo ", OUT=$OUT"; else echo ""; fi
	(cd $d && eval "$@") || echo "*************** $d FAIL ***************"
    done
}

xdate () { date "+$1 %H:%M:%S"; }
comp_hbc () { recurse "make clean_hbc && make hbc"; }
comp_ghc () { recurse "make clean_ghc && make ghc"; }
comp_hgc () { recurse 'make clean_hgc && xdate "START:" && make hgc >errl$OUT && xdate "STOP: "'; }
link_hgc () { recurse "make link"; }

run_hbc () { recurse "make run_hbc"; }
run_ghc () { recurse "make run_ghc"; }
run_hgc () { recurse "make run_hgc"; }

time_hbc () { recurse "make time_hbc"; }
time_ghc () { recurse "make time_ghc"; }
time_hgc () { recurse "make time_hgc"; }
repeat5 () { local i=0; while [ $i -lt 5 ]; do eval "$@"; i=$((i+1)); done; }
time5_hbc () { recurse "repeat5 'make time_hbc'"; }
time5_ghc () { recurse "repeat5 'make time_ghc'"; }
time5_hgc () { recurse "repeat5 'make time_hgc'"; }
time_all () { recurse "repeat5 'make time_hbc' && repeat5 'make time_ghc' && repeat5 'make time_hgc'"; }
time_all2 () {
    grep '%' "$1" | awk -Fu 'BEGIN { i = 0; j = 0; }
			     { if (i < 5) { x[i] = $1; i += 1; } else
		                          { printf("%6.2f %6.2f %6.2f %6.2f %6.2f\n",x[0],x[1],x[2],x[3],x[4]);
			                    i = 0; j += 1; { if (j == 3) { printf("\n"); j = 0; } };
			                    x[i] = $1; i += 1; }
			     }
                             END { printf("%6.2f %6.2f %6.2f %6.2f %6.2f\n",x[0],x[1],x[2],x[3],x[4]); }'
}

count_hbc () { recurse "make count_hbc"; }
count_ghc () { recurse "make count_ghc"; }
count_hgc () { recurse "make count_hgc"; }
spix_hgc () { recurse "make spix_hgc"; }
counts () { grep '^Instructions' "$@" | awk '{print $2;}'; }
counts_all () { (counts errl.count_hgc; counts errl.count_ghc; counts errl.count_hbc) | column -c 60; }

size_hgc () { recurse "make size_hgc"; }
summary_hgc () { recurse "make summary_hgc"; }
summary () { grep 'Codegen\|Coalesce\|Simplify\|Select\|graph size' "$@"; }
wc_hgc () { recurse "make wc_hgc"; }
stats_hgc () {
    local tmp=$SUBDIRS
    delete spectral/hartel/typecheck
    XHGC="-wall" comp_hgc
    SUBDIRS=spectral/hartel/typecheck
    XHGC="-wall -H300M" comp_hgc
    SUBDIRS=$tmp
    wc_hgc &> errl.wc_hgc
}

# HGXC='-xxx' comp_hgc:
evals () { recurse 'grep "STATS:" errl'; }

clean_all () { recurse make clean; }
clean_hbc () { recurse make clean_hbc; }
clean_ghc () { recurse make clean_ghc; }
clean_hgc () { recurse make clean_hgc; }
clean_hgc_all () {
    recurse make clean_hgc_all
    recurse 'for i in 0 1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n; do make OUT="$i" clean_hgc_all; done'
    recurse 'for i in g? a??; do make OUT="$i" clean_hgc_all; done'
}

test_hgc () { recurse "make hgc2 && make run_hgc && repeat5 'make time_hgc' && make count_hgc"; }

make_tar () { tar -zcvf benchmarks.tar.gz --exclude CVS --exclude .cvsignore README comp.sh hgc.mk results/errl.* $SUBDIRS; }

#------------------------------------------------------------------------------
# hbc/ghc:

bo1 () {
    comp_hbc &> errl.comp_hbc
    comp_ghc &> errl.comp_ghc
}

bo2 () {
    time5_hbc &> errl.time5_hbc
    time5_ghc &> errl.time5_ghc
}

bo3 () {
    count_hbc &> errl.count_hbc
    count_ghc &> errl.count_ghc
}

bother () { bo1 && bo2 && bo3; }

#------------------------------------------------------------------------------
# decision trees:

b1 () {
    OUT=1 XHGC="-fcase-max-tree 0" comp_hgc
    OUT=2 XHGC="-fcase-max-tree 3" comp_hgc
    OUT=3 XHGC="-fcase-max-tree 7" comp_hgc
    OUT=4 XHGC="-fcase-max-tree 15" comp_hgc
    OUT=5 XHGC="-fcase-max-tree 100" comp_hgc
}

b2 () {
    recurse 'for i in 1 2 3 4 5; do make OUT="$i" link; done'
    recurse 'for i in 1 2 3 4 5; do make OUT="$i" run_hgc; done'
    recurse 'for i in 1 2 3 4 5; do make OUT="$i" count_hgc; done'
    recurse 'for i in 1 2 3 4 5; do OUT="$i" repeat5 "make time_hgc"; done'
}

#------------------------------------------------------------------------------
# GRIN:

g1 () {
    local tmp=$SUBDIRS
    OUT=g0 comp_hgc
    delete spectral/boyer2 # heap
    OUT=g1 XHGC="-fno-case-sparse" comp_hgc
    SUBDIRS=$tmp
    OUT=g2 XHGC="-fno-case-eval" comp_hgc
    OUT=g3 XHGC="-fno-case-uneval" comp_hgc
    OUT=g4 XHGC="-fno-case-copy" comp_hgc
    OUT=g5 XHGC="-fno-unbox" comp_hgc
    OUT=g6 XHGC="-fno-update-elim" comp_hgc
    OUT=g7 XHGC="-fno-update-clos" comp_hgc
    OUT=g8 XHGC="-fno-dead-args" comp_hgc
    OUT=g9 XHGC="-fno-inline" comp_hgc
    delete spectral/boyer2 # heap
    OUT=ga XHGC="-fno-cse" comp_hgc
    SUBDIRS=$tmp
    OUT=gb XHGC="-fno-case-hoist" comp_hgc
    OUT=gc XHGC="-fno-arity-raise" comp_hgc
    OUT=gd XHGC="-fno-fetch-hoist" comp_hgc
    delete other/words # needs rematerialisation (long strings + CSE => alloc breaks).
    delete spectral/boyer2 # 13K string
    delete spectral/cichelli # hclaim
    delete spectral/awards # spill
    delete spectral/hartel/typecheck # spill
    OUT=ge XHGC="-fno-const-store" comp_hgc
    SUBDIRS=$tmp
    OUT=gf XHGC="-fno-updates" comp_hgc
    #
    SUBDIRS=spectral/boyer2 OUT=g1 XHGC="-fno-case-sparse -wall -H250M" comp_hgc
    SUBDIRS=$tmp
}

g2 () {
    recurse 'for i in g0 g1 g2 g3 g4 g5 g6 g7 g8 g9 ga gb gc gd ge gf; do make OUT="$i" link; done'
    recurse 'for i in g0 g1 g2 g3 g4 g5 g6 g7 g8 g9 ga gb gc gd ge gf; do make OUT="$i" run_hgc; done'
    recurse 'for i in g0 g1 g2 g3 g4 g5 g6 g7 g8 g9 ga gb gc gd ge gf; do make OUT="$i" size_hgc; done'
}

g3 () {
    recurse 'for i in g0 g1 g2 g3 g4 g5 g6 g7 g8 g9 ga gb gc gd ge gf; do make OUT="$i" count_hgc; done'
}

g4 () {
    recurse 'for i in g0 g1 g2 g3 g4 g5 g6 g7 g8 g9 ga gb gc gd ge gf; do OUT="$i" repeat5 "make time_hgc"; done'
}

g5 () {
    # heap stats (relink!):
    recurse 'for i in g0 g1 g2 g3 g4 g5 g6 g7 g8 g9 ga gb gc gd ge gf; do make OUT="$i" link; done'
    recurse 'for i in g0 g1 g2 g3 g4 g5 g6 g7 g8 g9 ga gb gc gd ge gf; do make OUT="$i" run_hgc; done'
}

# case hoisting aggressiveness:
gc1 () {
    local tmp=$SUBDIRS
    OUT=gg XHGC="-fcase-hoist-num 0" comp_hgc # default
    OUT=gh XHGC="-fcase-hoist-num 1" comp_hgc
    delete spectral/clausify # heap
    OUT=gi XHGC="-fcase-hoist-num 2" comp_hgc
    #
    # check: SUBDIRS=spectral/clausify OUT=gi XHGC="-fcase-hoist-num 2 -H250M" comp_hgc
    SUBDIRS=$tmp
}

gc2 () {
    recurse 'for i in gg gh gi; do make OUT="$i" link; done'
    recurse 'for i in gg gh gi; do make OUT="$i" run_hgc; done'
    recurse 'for i in gg gh gi; do make OUT="$i" size_hgc; done'
    recurse 'for i in gg gh gi; do make OUT="$i" count_hgc; done'
    recurse 'for i in gg gh gi; do OUT="$i" repeat5 "make time_hgc"; done'
}

gc3 () {
    # heap stats (relink!):
    recurse 'for i in gg gh gi; do make OUT="$i" link; done'
    recurse 'for i in gg gh gi; do make OUT="$i" run_hgc; done'
}

# Common sub-expression elimination:
gcse1 () {
    OUT=gj XHGC="" comp_hgc # default
    OUT=gk XHGC="-fno-cse-reset-app" comp_hgc
}

gcse2 () {
    recurse 'for i in gj gk; do make OUT="$i" link; done'
    recurse 'for i in gj gk; do make OUT="$i" run_hgc; done'
    recurse 'for i in gj gk; do make OUT="$i" count_hgc; done'
}

gx () {
    g1 &> errl.g1
    gc1 &> errl.gc1
    gcse1 &> errl.gcse1
}

gxr () {
    g2 &> errl.g2
    g3 &> errl.g3
    g4 &> errl.g4
    gc2 &> errl.gc2
    gcse2 &> errl.gcse2
}

#------------------------------------------------------------------------------
# alloc:

# coalesce restrictions:
a1 () {
    OUT=1 XHGC="-fcoalesce-restrict 0" comp_hgc # conservative
    OUT=2 XHGC="-fcoalesce-restrict 1" comp_hgc # only first build (default)
    OUT=3 XHGC="-fcoalesce-restrict 2" comp_hgc # no restriction
}

a1r () {
    recurse 'for i in 1 2 3; do make OUT="$i" link; done'
    recurse 'for i in 1 2 3; do make OUT="$i" run_hgc; done'
    recurse 'for i in 1 2 3; do make OUT="$i" count_hgc; done'
    recurse 'for i in 1 2 3; do make OUT="$i" summary_hgc; done'
}

# coalesce splitted locals:
a2 () {
    OUT=4 XHGC="-fcoalesce-splitted 0" comp_hgc # conservative (default)
    OUT=5 XHGC="-fcoalesce-splitted 1" comp_hgc # never
#    OUT=6 XHGC="-fcoalesce-splitted 2" comp_hgc # no restriction, does not work!
}

a2r () {
    recurse 'for i in 4 5; do make OUT="$i" link; done'
    recurse 'for i in 4 5; do make OUT="$i" run_hgc; done'
    recurse 'for i in 4 5; do make OUT="$i" count_hgc; done'
    recurse 'for i in 4 5; do make OUT="$i" summary_hgc; done'
}

# procedure weights:
# no differences at all!
a3 () {
    OUT=7 XHGC="-fspill-weight 2"  comp_hgc # 
    OUT=8 XHGC="-fspill-weight 5"  comp_hgc # default
    OUT=9 XHGC="-fspill-weight 10" comp_hgc # 
}

a3r () {
    recurse 'for i in 7 8 9; do make OUT="$i" link; done'
    recurse 'for i in 7 8 9; do make OUT="$i" run_hgc; done'
    recurse 'for i in 7 8 9; do make OUT="$i" count_hgc; done'
    recurse 'for i in 7 8 9; do make OUT="$i" summary_hgc; done'
}

# shrink-wrapping:     ("-fno-shrink-wrap-ret","  Do not shrink-wrap optimise the return address."),
a4 () {
    OUT=a XHGC="" comp_hgc
    OUT=b XHGC="-fno-shrink-wrap-ret" comp_hgc
}

a4r () {
    recurse 'for i in a b; do make OUT="$i" link; done'
    recurse 'for i in a b; do make OUT="$i" run_hgc; done'
    recurse 'for i in a b; do make OUT="$i" count_hgc; done'
    recurse 'for i in a b; do make OUT="$i" summary_hgc; done'
}

# spill costs for argument/return registers:
a5 () {
    OUT=c XHGC="" comp_hgc
    OUT=d XHGC="-fspill-argscosts" comp_hgc
}

a5r () {
    recurse 'for i in c d; do make OUT="$i" link; done'
    recurse 'for i in c d; do make OUT="$i" run_hgc; done'
    recurse 'for i in c d; do make OUT="$i" count_hgc; done'
    recurse 'for i in c d; do make OUT="$i" summary_hgc; done'
}

# number of registers:
a6 () {
    OUT=a61 XHGC="-r 20" comp_hgc # default
    OUT=a62 XHGC="-r 19" comp_hgc # default
    OUT=a63 XHGC="-r 18" comp_hgc # default
    OUT=a64 XHGC="-r 17" comp_hgc # default
    OUT=a65 XHGC="-r 16" comp_hgc # default
    OUT=a66 XHGC="-r 15" comp_hgc # default
    OUT=a67 XHGC="-r 14" comp_hgc # default
}

a6r () {
    recurse 'for i in a61 a62 a63 a64 a65 a66 a67; do make OUT="$i" link; done'
    recurse 'for i in a61 a62 a63 a64 a65 a66 a67; do make OUT="$i" run_hgc; done'
    recurse 'for i in a61 a62 a63 a64 a65 a66 a67; do make OUT="$i" spix_hgc; done'
    recurse 'for i in a61 a62 a63 a64 a65 a66 a67; do make OUT="$i" summary_hgc; done'
}

a7 () {
    OUT=a71 XHGC="-r 13" comp_hgc
    OUT=a72 XHGC="-r 12" comp_hgc
    OUT=a73 XHGC="-r 11" comp_hgc
    OUT=a74 XHGC="-r 10" comp_hgc
    OUT=a75 XHGC="-r  9" comp_hgc
    OUT=a76 XHGC="-r  8" comp_hgc
}

a7r () {
    recurse 'for i in a71 a72 a73 a74 a75 a76; do make OUT="$i" link; done'
    recurse 'for i in a71 a72 a73 a74 a75 a76; do make OUT="$i" run_hgc; done'
    recurse 'for i in a71 a72 a73 a74 a75 a76; do make OUT="$i" spix_hgc; done'
    recurse 'for i in a71 a72 a73 a74 a75 a76; do make OUT="$i" summary_hgc; done'
}

a8 () {
    local tmp=$SUBDIRS
    SUBDIRS="spectral/sorting spectral/cichelli spectral/boyer2"
    OUT=a81 XHGC="-r 7" comp_hgc
    OUT=a82 XHGC="-r 6" comp_hgc
    OUT=a83 XHGC="-r 5" comp_hgc
    SUBDIRS=$tmp
}

a8r () {
    local tmp=$SUBDIRS
    SUBDIRS="spectral/sorting spectral/cichelli spectral/boyer2"
    recurse 'for i in a81 a82 a83; do make OUT="$i" link; done'
    recurse 'for i in a81 a82 a83; do make OUT="$i" run_hgc; done'
    recurse 'for i in a81 a82 a83; do make OUT="$i" spix_hgc; done'
    recurse 'for i in a81 a82 a83; do make OUT="$i" summary_hgc; done'
    SUBDIRS=$tmp
}

ax () {
    a1 &> errl.a1
    a2 &> errl.a2
#    a3r &> errl.a3r
    a4 &> errl.a4
    a5 &> errl.a5
    a6 &> errl.a6
    a7 &> errl.a7
}

axr () {
    a1r &> errl.a1r
    a2r &> errl.a2r
#    a3r &> errl.a3r
    a4r &> errl.a4r
    a5r &> errl.a5r
    a6r &> errl.a6r
    a7r &> errl.a7r
}

#      ("-fsplits-no-select","    Skip select if splits found."),
#      ("-fsplits-use-origs1","   Split original split candidates if select fails with splits."),
#      ("-fsplits-use-origs2","   Split original split candidates if select fails for any reason.")
