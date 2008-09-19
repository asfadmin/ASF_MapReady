# This makefile builds all the tools in the asf_tools/contrib directory
# (except xid) that were formerly on cedar

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
