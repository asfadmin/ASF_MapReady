# asf_import

include ../../make_support/system_rules

LIBS  = \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf.a \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	-lz

CFLAGS += \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

# These defines enable various debugging behavior and output.  Leaving
# debugging code in and available is all the rage these days, and is
# probably even a good idea.  The following stuff constitute a sort of
# catalog of the available debugging features for this program, which
# acronymish things you might choose to enable from the command line
# that invokes make (e.g. make DISSJO=nonemptystring) so you don't
# forget to remove them before release.
#
DISSJO =
ifdef DIUSJO
  DEBUG_FEATURES += -DDEBUG_IMPORT_USGS_SEAMLESS_JPEG_OUTPUT
endif
DIAUG =
ifdef DIAUG
  DEBUG_FEATURES += -DDEBUG_IMPORT_ASF_UTM_GEOTIFF_JPEG_OUTPUT
endif
CFLAGS += $(DEBUG_FEATURES)

OBJS  = asf_import.o

all: asf_import
	-rm *.o

# It looks like linking against efence triggers a bug in libgeotiff on
# solaris, so we forbid that.
asf_import: $(OBJS)
	perl -e 'exit("$(SYS)" eq "solaris" and "$(LDFLAGS)" =~ m/-lefence/);'
	$(CC) $(CFLAGS) -o asf_import $(OBJS) $(LIBS) $(LDFLAGS)
	mv asf_import$(BIN_POSTFIX) $(BINDIR)

clean:
	rm -f core $(OBJS) *.o
