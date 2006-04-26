# Makefile for libasf_sar library

include ../../make_support/system_rules

CFLAGS := -Wall $(CFLAGS)

OBJS = \
	gr2sr.o

LIBS = \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_sar.a \
	$(LDFAGS) \
	-lm

all: build_only
	cp libasf_sar.a $(LIBDIR)
	cp asf_sar.h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f libasf_sar.a
	ar r libasf_sar.a $(OBJS)
	$(RANLIB) libasf_sar.a

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h)

clean:
	rm -rf $(OBJS) libasf_sar.a
