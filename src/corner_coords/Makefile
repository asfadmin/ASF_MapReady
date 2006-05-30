# Makefile for corner_coords
# Author Rudi Gens

include ../../make_support/system_rules

CFLAGS += $(GLIB_CFLAGS)

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	-lm

OBJS = corner_coords.o

OBJS2 = latLon2proj.o

OBJS3 =	proj2latLon.o

OBJS4 = proj2proj.o

all: corner_coords latLon2proj proj2latLon proj2proj clean

corner_coords: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o corner_coords $(OBJS) $(LIBS)
	mv corner_coords$(BIN_POSTFIX) $(BINDIR)
#	cp corner_coords.1 ../../man/man1/corner_coords.1

latLon2proj: $(OBJS2)
	$(CC) $(CFLAGS) $(LDFLAGS) -o latLon2proj $(OBJS2) $(LIBS)
	mv latLon2proj$(BIN_POSTFIX) $(BINDIR)

proj2latLon: $(OBJS3)
	$(CC) $(CFLAGS) $(LDFLAGS) -o proj2latLon $(OBJS3) $(LIBS)
	mv proj2latLon$(BIN_POSTFIX) $(BINDIR)

proj2proj: $(OBJS4)
	$(CC) $(CFLAGS) $(LDFLAGS) -o proj2proj $(OBJS4) $(LIBS)
	mv proj2proj$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f $(OBJS) $(OBJS2) $(OBJS3) $(OBJS4) core *~


