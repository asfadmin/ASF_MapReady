# This makefile builds all the tools in the JPL Mosaic Suite

MODULES = \
	ampscheby \
	detilt_hgt \
	dgx \
	match_20488nr \
	multimosaic30_20941nr

all:
	$(foreach MODULE, $(MODULES), $(MAKE) -C $(MODULE) &&) true

clean:
	$(foreach MODULE, $(MODULES), $(MAKE) -C $(MODULE) clean &&) true
	rm -rf bin/ include/ info/ lib/ man/ share/
