# Makefile for hdr2ddr
# Author Rudi Gens

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm

OBJS = hdr2ddr.o

all: hdr2ddr clean

hdr2ddr: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o hdr2ddr $(OBJS) $(LIBS) $(LDFLAGS)
	mv hdr2ddr$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS) core
