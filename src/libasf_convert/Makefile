# Makefile for libasf_convert library

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS)

OBJS  = asf_convert.o \
	config.o \
	functions.o

CFLAGS += $(GLIB_CFLAGS)

all: build_only
	mv libasf_convert.a $(LIBDIR)
	cp asf_convert.h $(ASF_INCLUDE_DIR)
	mkdir -p $(SHAREDIR)/asf_convert
	cp batch.config $(SHAREDIR)/asf_convert
	cp asf_convert.defaults $(SHAREDIR)/asf_convert
	cp asf_convert_cmd.pdf $(SHAREDIR)/asf_convert

build_only: $(OBJS)
	rm -f libasf_convert.a
	ar r libasf_convert.a $(OBJS)
	$(RANLIB) libasf_convert.a

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*h)

clean:
	rm -f core $(OBJS) *.o *~
