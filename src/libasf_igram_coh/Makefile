# Makefile for libasf_igram_coh library

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	-lz

CFLAGS += \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

OBJS  = asf_igram_coh.o

all: build_only
	mv libasf_igram_coh.a $(LIBDIR)
	cp asf_igram_coh.h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f libasf_igram_coh.a
	ar r libasf_igram_coh.a $(OBJS)
	$(RANLIB) libasf_igram_coh.a

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*h)

clean:
	rm -f core $(OBJS) *.o
