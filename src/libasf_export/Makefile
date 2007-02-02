# Makefile for libasf_export library

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

SOURCES := asf_export.c \
	export_band.c \
	export_as_envi.c \
	export_as_esri.c \
	util.c 

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

CFLAGS += \
	$(GEOTIFF_CFLAGS) \
	$(TIFF_CFLAGS) \
	$(JPEG_CFLAGS) \
	$(PROJ_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS) -Wall


LIBS  = \
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
	-lm \
	-lz


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

all: build_only
	mv libasf_export.a $(LIBDIR)
	cp asf_export.h $(ASF_INCLUDE_DIR)

build_only: $(OBJS)
	rm -f libasf_export.a
	ar r libasf_export.a $(OBJS)
	$(RANLIB) libasf_export.a

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*h)

clean:
	rm -rf $(OBJS) core.* core *~ libasf_export.a
