CFLAGS += $(HDF5_CFLAGS)
CFLAGS += $(GEOTIFF_CFLAGS)
PROGRAM := asf_export

include ../../make_support/system_rules

###############################################################################
#
# List of Sources
#
# Here are the variables that list all the things that need building.
# When new source files are added, something in here will need to
# change.
#
###############################################################################

SOURCES := asf_export.c

###############################################################################
#
# Libraries and Tools
#
# Here are variables which describe the libraries and tools needed by
# this module, and the flags required to compile code which uses them.
# If a new library or tool dependency is added, something in here will
# need to change.
#
###############################################################################

CFLAGS += $(W_ERROR) \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PNG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS) \
	$(XML_CFLAGS) \
	$(HDF5_CFLAGS)

LIBS  = \
	$(LIBDIR)/libasf_export.a \
	$(LIBDIR)/libasf_vector.a \
	$(LIBDIR)/libasf_import.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf_meta.a \
	$(SHAPELIB_LIBS) \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/asf.a \
	$(GEOTIFF_LIBS) \
	$(TIFF_LIBS) \
	$(JPEG_LIBS) \
	$(PNG_LIBS) \
	$(PROJ_LIBS) \
	$(XML_LIBS) \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(NETCDF_LIBS) \
	$(HDF5_LIBS) \
	$(ZLIB_LIBS) \
	-lm \

LDLIBS := $(LIBS)

###############################################################################
#
# Automaticly Computed Stuff
#
# The rest of this makefile fragment consists of stuff that uses the
# above stuff in ways that are unlikely to change too much, and isn't
# likely to need much modification.
#
###############################################################################

OBJS := $(patsubst %.c, %.o, $(SOURCES))

all: asf_export

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
	mv asf_export$(BIN_POSTFIX) $(BINDIR)

.PHONY: clean
clean:
	rm -f $(PROGRAM) $(OBJS) core.* core
