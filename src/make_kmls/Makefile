include ../../make_support/system_rules

TARGET = make_kmls

CFLAGS += $(GLIBS_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libshp.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf_fft.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(HDF5_LIBS) \
	$(PROJ_LIBS) \
	$(GSL_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(JPEG_LIBS) \
	$(XML_LIBS) \
	$(ZLIB_LIBS) \
	-lm

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS)

OBJS  = $(TARGET).o

all: prog
	-rm *.o

prog: $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LIBS) $(LDFLAGS)
	mv $(TARGET)$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
