# Makefile for libasf_convert library

include ../../make_support/system_rules

OBJS  = asf_convert.o \
	config.o \
	functions.o

CFLAGS += $(GLIB_CFLAGS) $(GSL_CFLAGS) $(PROJ_CFLAGS)

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
