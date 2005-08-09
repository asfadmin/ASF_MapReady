# Makefile
# Author Rudi Gens

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	-lm

HDR_OBJS = get_hdr.o \
	hdr2ddr.o \
	hdr2ddr_main.o

DDR_OBJS = ddr2hdr.o \
	ddr2hdr_main.o \
	put_hdr.o

all: hdr2ddr ddr2hdr clean

hdr2ddr: $(HDR_OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o hdr2ddr $(HDR_OBJS) $(LIBS) $(LDFLAGS)
	mv hdr2ddr$(BIN_POSTFIX) $(BINDIR)

ddr2hdr: $(DDR_OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o ddr2hdr $(DDR_OBJS) $(LIBS) $(LDFLAGS)
	mv ddr2hdr$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core
