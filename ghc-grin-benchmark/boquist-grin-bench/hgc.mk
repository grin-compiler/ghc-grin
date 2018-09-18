# $Id: hgc.mk,v 1.29 1999/02/23 10:49:51 boquist Exp $

.SUFFIXES: .hs .sx .s .hbc .ghc
.PRECIOUS: %.sx

hgc=$(HGCDIR)/bin/hgc

.hs.sx:
	$(hgc) -cpp -H30M $(HBCCFLAGS) $(XHBCC) $<

.sx.s:
	$(hgc) -A4M $(HGC1) $(HGC2) $(HGC3) $(XHGC) $<

hgc:	$(PROG).s
ifdef OUT
	mv $(PROG).s $(OUT).s
endif

link:
ifdef OUT
	$(hgc) $(OUT).s
else
	$(hgc) $(PROG).s
endif

hbc=hbc -static -O -msparc8

.hs.hbc:
	$(hbc) -cpp -o $@ $(HBCFLAGS) $(XHBC) $<

hbc: $(PROG).hbc

ghc=env GCC_EXEC_PREFIX=/usr/pd/gnu/lib/ada/3.10p/lib/gcc-lib/sparc-sun-solaris2.6/2.7.2/ \
	ghc-4.01 -static -O2 -fvia-C -O2-for-C -mv8 

.hs.ghc:
	$(ghc) -cpp -o $@ $(GHCFLAGS) $(XGHC) $<

ghc: $(PROG).ghc

run_hgc:
ifdef OUT
	ulimit -t 30; ./$(OUT) $(RUNFLAGS) > run.out && cat run.out && cmp -s hgc.out run.out
else
	ulimit -t 30; ./$(PROG) $(RUNFLAGS) > run.out && cat run.out && cmp -s hgc.out run.out
endif

run_hbc:
	./$(PROG).hbc -A200k $(RUNFLAGS)

run_ghc:
	./$(PROG).ghc +RTS $(RUNFLAGS)

time_hgc:
ifdef OUT
	tcsh -f -c "limit cputime 30; time ./$(OUT) $(RUNFLAGS)"
else
	tcsh -f -c "limit cputime 30; time ./$(PROG) $(RUNFLAGS)"
endif

time_hbc:
	tcsh -f -c "time ./$(PROG).hbc -A200k $(RUNFLAGS)"

time_ghc:
	tcsh -f -c "time ./$(PROG).ghc +RTS $(RUNFLAGS)"

size_hgc:
ifdef OUT
	size $(OUT).o
else
	size $(PROG).o
endif

summary_hgc:
	@grep 'Codegen\|Coalesce\|Simplify\|Select\|graph size' errl$(OUT)
	@grep 'TIME' errl$(OUT)
	@echo

wc_hgc:
ifdef OUT
	wc -l $(OUT).4.is       # ISyntax
	wc -l $(OUT).6.g        # Initial GRIN
	wc -l $(OUT).11.g       # Eval inlining
	wc -l $(OUT).94.g       # Final GRIN
	wc -l $(OUT).R          # Initial RISC
	wc -l $(OUT).O.m.s      # Final RISC
	grep 'time\|iterations' $(OUT).7.g # Flow analysis
else
	wc -l $(PROG).4.is      # ISyntax
	wc -l $(PROG).6.g       # Initial GRIN
	wc -l $(PROG).11.g      # Eval inlining
	wc -l $(PROG).94.g      # Final GRIN
	wc -l $(PROG).R         # Initial RISC
	wc -l $(PROG).O.m.s     # Final RISC
	grep 'time\|iterations' $(PROG).7.g # Flow analysis
endif

SHADE=/users/cs/boquist/work/shadekit/shade.v9.elf
SPIX=/users/cs/boquist/work/shadekit/spixtools.v9.elf
count=$(SHADE)/bin/icount
split=$(SPIX)/bin/,

count_hgc:
ifdef OUT
	ulimit -t 300; $(count) -- ./$(OUT) $(RUNFLAGS) | $(split)
else
	ulimit -t 300; $(count) -- ./$(PROG) $(RUNFLAGS) | $(split)
endif

count_hbc:
	$(count) -- ./$(PROG).hbc -A200k $(RUNFLAGS) | $(split)

count_ghc:
	$(count) -- ./$(PROG).ghc +RTS $(RUNFLAGS) | $(split)

spixcounts=$(SHADE)/bin/spixcounts
spixstats=$(SPIX)/bin/spixstats
sdas=$(SPIX)/bin/sdas

spix_hgc:
ifdef OUT	
	ulimit -t 700; $(spixcounts) -b '%p.spix.bb' -- $(OUT) $(RUNFLAGS) | $(split)
	$(spixstats) -v -b $(OUT).spix.bb $(OUT) > $(OUT).spix.out
	grep 'stw\|lduw\|^TOTAL' $(OUT).spix.out | head -3
	$(sdas) -b $(OUT).spix.bb $(OUT) > $(OUT).spix.sdas
	/users/cs/boquist/work/alloc/examples/grinstack $(OUT).spix.sdas
else
	ulimit -t 700; $(spixcounts) -b '%p.spix.bb' -- $(PROG) $(RUNFLAGS) | $(split)
	$(spixstats) -v -b $(PROG).spix.bb $(PROG) > $(PROG).spix.out
	grep 'stw\|lduw\|^TOTAL' $(PROG).spix.out | head -3
	$(sdas) -b $(PROG).spix.bb $(PROG) > $(PROG).spix.sdas
	/users/cs/boquist/work/alloc/examples/grinstack $(PROG).spix.sdas
endif

clean: clean_hgc_all clean_hbc clean_ghc

clean_hgc_all: clean_hgc clean_hgc_bin clean_spix
	@rm -f ap* bep* cep* dep* ep* fep* errl* a.out
	@rm -f *.o *.R *.R.* *.A.* *.O.* *.h *.gc *.s *.sx *.g *.is
	@rm -f STAT.* *.ghc.stat *.hi *.oi *.vcg *.old run.out
	@rm -f $(PROG)_hgc.hs	# boyer2

clean_hgc:
	@rm -f $(PROG).sx $(PROG).s $(OUT).s

clean_hgc_bin:
	@rm -f $(PROG) $(OUT)

clean_spix:
	@rm -f *.spix *.spix.*

clean_hbc:
	@rm -f *.hbc STAT.* *.hi *.o Main

clean_ghc:
	@rm -f *.ghc *.ghc.stat
