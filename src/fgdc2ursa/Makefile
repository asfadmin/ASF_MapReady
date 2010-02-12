# Makefile for fgdc2ursa

DEST_TARGET = fgdc2ursa

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/libasf_proj.a \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(PROJ_LIBS) \
        $(TIFF_LIBS) \
        $(JPEG_LIBS) \
        $(PNG_LIBS) \
        $(GEOTIFF_LIBS) \
	$(XML_LIBS) \
	$(ZLIB_LIBS)

CFLAGS += $(XML_CFLAGS) -D REENTRANT

OBJS  = fgdc2ursa.o

all:    fgdc2ursa clean

fgdc2ursa: $(OBJS)
	$(CC) $(CFLAGS) -o fgdc2ursa $(OBJS) $(LIBS) $(LDFLAGS)
	mv fgdc2ursa$(BIN_POSTFIX) $(BINDIR)/$(DEST_TARGET)$(BIN_POSTFIX)

clean:
	rm -f core $(OBJS) *.o *~
