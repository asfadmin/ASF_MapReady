######################################################################
# Makefile for 		:  fftMatch.c 
# Module Author 	:  Orion Lawlor
# Module Version	:  1.0
######################################################################

include ../../make_support/system_rules

LIBS =	\
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	-lm 
CFLAGS += $(PROJ_CFLAGS) $(GSL_CFLAGS) $(GLIB_CFLAGS)

all: fftMatch clean

fftMatch: $(OBJLIB) $(LIBS)
	$(CC) $(CFLAGS) -o fftMatch fftMatch.c $(LIBS) $(LDFLAGS)
	mv fftMatch$(BIN_POSTFIX) $(BINDIR)
	cp fftMatch.man ../../man/cat1/fftMatch.1

clean:
	rm -f *.o
