# Makefile for libasf_terrcorr library

include ../../make_support/system_rules

CFLAGS := -Wall $(CFLAGS)

OBJS = libasf_terrcorr.o

LIBS  = \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(GSL_LIBS) \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

all: build_only
	cp libasf_terrcorr.a $(LIBDIR)
	cp asf_terrcorr.h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f libasf_terrcorr.a
	ar r libasf_terrcorr.a $(OBJS)
	$(RANLIB) libasf_terrcorr.a

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)

clean:
	rm -rf $(OBJS) libasf_terrcorr.a *~
