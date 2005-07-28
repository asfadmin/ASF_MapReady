######################################################################
# Makefile for 		:  atdp.c 
# Module Author 	:  Rudi Gens
# Module Version	:  1.0
######################################################################

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_odl.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libproj.a \
	$(LIBDIR)/libasf_reporting.a
LIBC = $(LIBS) -lm 

ASPLIB = atdp_setup.o

# Filter out the ISOC99 FLAGS which causes some problems.
CFLAGS := $(patsubst -D_ISOC99_SOURCE, , $(CFLAGS))

all: atdp clean

atdp:	$(ASPLIB) atdp.o $(LIBS)
	$(CC) $(CFLAGS) -o atdp atdp.o $(ASPLIB) $(LIBC) $(LDFLAGS)
	mv atdp$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
