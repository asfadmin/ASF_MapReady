CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
######################################################################
# Makefile for 		:  fftMatch.c 
# Module Author 	:  Orion Lawlor
# Module Version	:  1.0
######################################################################

include ../../make_support/system_rules

LIBS =	\
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf.a \
	$(GDAL_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(ZLIB_LIBS) \
	-lm 

CFLAGS += $(PROJ_CFLAGS) $(GSL_CFLAGS) $(GLIB_CFLAGS)

all: fftMatch fftMatchGrid fftMatchProj fftMatchOptical clean

fftMatch: $(OBJLIB)
	$(CC) $(CFLAGS) -o fftMatch fftMatch.c $(LIBS) $(LDFLAGS)
	mv fftMatch$(BIN_POSTFIX) $(BINDIR)
	cp fftMatch.man ../../man/cat1/fftMatch.1

fftMatchGrid: $(OBJLIB)
	$(CC) $(CFLAGS) -o fftMatchGrid fftMatchGrid.c $(LIBS) $(LDFLAGS)
	mv fftMatchGrid$(BIN_POSTFIX) $(BINDIR)

fftMatchProj: $(OBJLIB)
	$(CC) $(CFLAGS) -o fftMatchProj fftMatchProj.c $(LIBS) $(LDFLAGS)
	mv fftMatchProj$(BIN_POSTFIX) $(BINDIR)

fftMatchOptical: $(OBJLIB)
	$(CC) $(CFLAGS) -o fftMatchOptical fftMatchOptical.c $(LIBS) $(LDFLAGS)
	mv fftMatchOptical$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f *.o
