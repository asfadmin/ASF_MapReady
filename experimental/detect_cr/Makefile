# Makefile for detect_cr
# Module Author: Rudi Gens (sliced and diced from coregister_fine,
#	                    coregister_coarse and fftMatch)

include ../../make_support/system_rules

LIBS  = $(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/asf_fft.a \
	-lm

OBJS  = detect_cr.o \
	utilities.o

all: detect_cr clean

detect_cr: $(OBJS)
	$(CC) $(CFLAGS) -o detect_cr $(OBJS) $(LIBS) $(LDFLAGS)
	cp detect_cr$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
