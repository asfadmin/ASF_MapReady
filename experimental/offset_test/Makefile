# Makefile for offset_test
# Module Author: Rudi Gens (sliced and diced from coregister_fine, 
#	                    coregister_coarse and fftMatch)


include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/asf_fft.a \
	-lm

OBJS  = offset_test.o \
	fft_corr.o
FOBJS = test_fopen.o

all: offset_test test_fopen clean

offset_test: $(OBJS)
	$(CC) $(CFLAGS) -o offset_test $(OBJS) $(LIBS) $(LDFLAGS)
	cp offset_test$(BIN_POSTFIX) $(BINDIR)

test_fopen: $(FOBJS)
	$(CC) $(CFLAGS) -o test_fopen $(FOBJS) $(LIBS) $(LDFLAGS)
	cp test_fopen$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
