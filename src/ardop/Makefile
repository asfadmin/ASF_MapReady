include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_ardop.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(PROJ_LIBS) \
	$(JPEG_LIBS) \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(PNG_LIBS) \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(HDFEOS5_LIBS) \
	$(GDAL_LIBS) \
	$(XML_LIBS) \
	$(ZLIB_LIBS)

LIBC = $(LIBS) -lm 

OBJS = ardop.o parse_cla.o

all: ardop

ardop:	$(OBJS)
	$(CC) $(CFLAGS) -o ardop $(OBJS) $(LIBC) $(LDFLAGS)
	mv ardop$(BIN_POSTFIX) $(BINDIR)
	cp ardop.1 ../../man/cat1/ardop.1 

clean:
	rm -f core *.o
