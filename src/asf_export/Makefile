# Makefile for asf_export
# Author Britton Kerin, June 2004
#    mod Patrick Denny, June 2004

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgeotiff.a \
	$(LIBDIR)/libtiff.a \
	$(LIBDIR)/libjpeg.a \
	$(LIBDIR)/libgsl.a \
	-lm

OBJS  = asf_export.o

all: asf_export

asf_export: $(OBJS)
	$(CC) $(CFLAGS) $(LFLAGS) -o asf_export  $(OBJS) $(LIBS) $(LDFLAGS)
	cp asf_export$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o core.* asf_export$(BIN_POSTFIX)
