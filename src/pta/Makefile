# Makefile for pslr
# Module Author: Rudi Gens

DEBUG_BUILD = 2

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/asf_fft.a \
	-lm

OBJS  = pta.o \
	utilities.o

all: pta clean

pta: $(OBJS)
	$(CC) $(CFLAGS) -o pta $(OBJS) $(LIBS) $(LDFLAGS)
	cp pta$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
