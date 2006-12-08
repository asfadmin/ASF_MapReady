# Makefile for libasf_insar library

include ../../make_support/system_rules

CFLAGS := -Wall $(CFLAGS) $(GSL_CFLAGS)

OBJS = \
	complex2polar.o \
	polar2complex.o \
	baseline.o

LIBS = \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LDFLAGS) \
	$(GSL_LIBS) \
	-lm

all: build_only
	cp libasf_insar.a $(LIBDIR)
	cp asf_insar.h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f libasf_insar.a
	ar r libasf_insar.a $(OBJS)
	$(RANLIB) libasf_insar.a

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)

clean:
	rm -rf $(OBJS) libasf_insar.a *~
