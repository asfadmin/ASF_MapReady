# Makefile for program image_layers
# by Rudi Gens, ASF, 2003
# Patrick Denny, March 2004
# Ingests the complete variety of CEOS data into ASF internal format

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf.a \
	-lm

OBJS  = least_squares.o \
	matrix.o

all: image_layers clean

image_layers: $(OBJS) image_layers.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o image_layers image_layers.c $(OBJS) $(LIBS)
	mv image_layers$(BIN_POSTFIX) $(BINDIR)
#	cp image_layers.1 ../../man/cat1/

# This is an old version that calculates er&ht every single time & is slow
# Kept as 'experimental' code
image_layers_org: $(OBJS) image_layers_org.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o image_layers_org image_layers_org.c $(OBJS) $(LIBS)
	mv image_layers_org$(BIN_POSTFIX) $(BINDIR)
#	cp image_layers.1 ../../man/cat1/

clean:
	rm -f core *.o
