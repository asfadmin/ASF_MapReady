# Makefile for corner_coords
# Author Rudi Gens

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	-lm

OBJS = corner_coords.o

all: corner_coords clean

corner_coords: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o corner_coords $(OBJS) $(LIBS)
	mv corner_coords$(BIN_POSTFIX) $(BINDIR)
#	cp corner_coords.1 ../../man/man1/corner_coords.1

clean:
	rm -f $(OBJS)


