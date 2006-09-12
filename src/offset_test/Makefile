# Makefile for offset_test
# Module Author: Rudi Gens (sliced and diced from coregister_fine, 
#	                    coregister_coarse and fftMatch)


include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(GSL_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libshp.a \
	$(LIBDIR)/libifm.a \
	$(LIBDIR)/asf_fft.a \
	-lm

OBJS  = offset_test.o \
	fft_corr.o \
	utilities.o

all: offset_test clean

offset_test: $(OBJS)
	$(CC) $(CFLAGS) -o offset_test $(OBJS) $(LIBS) $(LDFLAGS)
	mv offset_test$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core *.o
