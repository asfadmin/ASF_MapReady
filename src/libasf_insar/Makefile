# Makefile for libasf_insar library

include ../../make_support/system_rules

CFLAGS := -Wall $(CFLAGS) $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS = \
	functions.o \
	ips_config.o \
	ips.o \
	complex2polar.o \
	polar2complex.o \
	baseline.o \
	asf_igram_coh.o \
	asf_elevation.o \
	asf_coregister.o

all: build_only
	mv libasf_insar.a $(LIBDIR)
	cp asf_insar.h $(ASF_INCLUDE_DIR)
	cp ips.h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f libasf_insar.a
	ar r libasf_insar.a $(OBJS)
	$(RANLIB) libasf_insar.a

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)

clean:
	rm -rf $(OBJS) libasf_insar.a *~
