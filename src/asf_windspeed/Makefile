CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
CFLAGS += $(HDF5_CFLAGS)
# asf_windspeed

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_sar.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf.a \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(ZLIB_LIBS) 

CFLAGS += \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

OBJS  = asf_windspeed.o

all: asf_windspeed
	-rm *.o

# It looks like linking against efence triggers a bug in libgeotiff on
# solaris, so we forbid that.
asf_windspeed: $(OBJS)
	perl -e 'exit("$(SYS)" eq "solaris" and "$(LDFLAGS)" =~ m/-lefence/);'
	$(CC) $(CFLAGS) -o asf_windspeed $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_windspeed$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o

