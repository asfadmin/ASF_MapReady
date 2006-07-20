# Makefile for libasf_elevation library

include ../../make_support/system_rules

DEBUG_BUILD = 2

LIBS  = \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	-lz

CFLAGS += \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

OBJS  = asf_elevation.o

all: build_only
	mv libasf_elevation.a $(LIBDIR)
	cp asf_elevation.h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f libasf_elevation.a
	ar r libasf_elevation.a $(OBJS)
	$(RANLIB) libasf_elevation.a

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*h)

clean:
	rm -f core $(OBJS) *.o
