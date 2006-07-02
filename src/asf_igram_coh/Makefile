# Makefile for asf_igram_coh 

include ../../make_support/system_rules

DEBUG_BUILD = 2

CFLAGS += $(GLIB_CFLAGS)

LIBS  = $(LIBDIR)/libasf_igram_coh.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	-lm
OBJS = asf_igram_coh.o

all: asf_igram_coh
	rm *.o

asf_igram_coh: $(OBJS)
	$(CC) $(CFLAGS) -o asf_igram_coh $(OBJS) $(LIBS) $(LDFLAGS)
	cp asf_igram_coh$(BIN_POSTFIX) $(BINDIR)
