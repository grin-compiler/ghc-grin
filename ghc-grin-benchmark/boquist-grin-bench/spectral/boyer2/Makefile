PROG=boyer2
HBCCFLAGS=-fno-listy-trans
HGC1=-dall
HGC2=-H150M
HGC3=-fstrings
HBCFLAGS=
GHCFLAGS=
RUNFLAGS=-H10M

include ../../hgc.mk

# XXX: the C preprocessor breaks Haskell multiline strings!

# Add hbcc Prelude manually:
boyer2.s: boyer2.hs
	cat boyer2.hs prel.hs | sed 's/putStr/xprint/' > boyer2_hgc.hs
	$(hgc) -H30M $(HBCCFLAGS) $(XHBCC) boyer2_hgc.hs
	$(hgc) -A1M $(HGC1) $(HGC2) $(HGC3) $(XHGC) boyer2_hgc.sx
	mv boyer2_hgc.s boyer2.s

# Remove "-cpp" from default rules:
boyer2.hbc: boyer2.hs
	$(hbc) -o $@ $(HBCFLAGS) $(XHBC) $<

boyer2.ghc: boyer2.hs
	$(ghc) -o $@ $(GHCFLAGS) $(XGHC) $<
