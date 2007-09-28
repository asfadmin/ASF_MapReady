# This makefile builds all the tools in the asf_tools/contrib directory
# (except xid) that were formerly on cedar

MODULES = \
	ampscheby \
	detilt_hgt \
	dgx/src \
	match_20488nr/src \
	multimosaic30_20941nr/src

all:
	$(foreach MODULE, $(MODULES), $(MAKE) -C $(MODULE) &&) true

clean:
	$(foreach MODULE, $(MODULES), $(MAKE) -C $(MODULE) clean &&) true
	rm -rf bin/ include/ info/ lib/ man/ share/
