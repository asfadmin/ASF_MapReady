# Makefile for offset_test
# Module Author: Rudi Gens (sliced and diced from coregister_fine, coregister_coarse
#                           and fftMatch)

DEBUG = 2

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/asf_fft.a \
	-lm

OBJS  = offset_test.o

all: offset_test clean

offset_test: $(OBJS)
	$(CC) $(CFLAGS) -o offset_test $(OBJS) $(LIBS) $(LDFLAGS)
	cp offset_test$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
