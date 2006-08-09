
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

SOURCES := geocode_options.c \
	   geocode_dem.c \
	   set_projection_functions.c \
           parse_options.c \
           check_parameters.c \
           asf_geocode.c \
	   geoid.c

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

CC := gcc

INCLUDE_FLAGS := -I$(ASF_INCLUDE_DIR)

CPPFLAGS := $(INCLUDE_FLAGS)

# I'd like to use -Wconversion as advised by the GSL, but it seems to
# make it impossible to pass arguments of type float to functions that
# take type float (still get a complaint about argument being passed
# as float due to prototype rather than double.
#          -Wconversion -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align
CFLAGS += $(GSL_CFLAGS) $(JPEG_CFLAGS) $(PROJ_CFLAGS) $(GLIB_CFLAGS) \
	-Werror -Wall -W -Wmissing-prototypes -Wstrict-prototypes \
	-Wshadow -Wpointer-arith -Wcast-qual -Wcast-align \
	-Wwrite-strings -Wnested-externs -fno-common

LIBS :=	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	-lm

# These defines enable various debugging behavior and output.  Leaving
# debugging code in and available is all the rage these days, and is
# probably even a good idea.  The following stuff constitute a sort of
# catalog of the available debugging features for this program, which
# acronymish things you might choose to enable from the command line
# that invokes make (e.g. make DISSJO=nonemptystring) so you don't
# forget to remove them before release.
#
DGDZCIAJ =
ifdef DGDZCIAJ
  DEBUG_FEATURES += -DDEBUG_GEOCODE_DEM_Z_COORDS_IMAGE_AS_JPEG
endif
DGDOIAJ =
ifdef DGDOIAJ
  DEBUG_FEATURES += -DDEBUG_GEOCODE_DEM_OUTPUT_IMAGE_AS_JPEG
endif
CFLAGS += $(DEBUG_FEATURES)

###############################################################################
#
# Automaticly Computed Stuff
#
# The rest of this makefile fragment consists of stuff that uses the
# above stuff in ways that are unlikely to change too much, and isn't
# likely to need much modification.
#
###############################################################################

# Object files.
OBJS := $(patsubst %.c, %.o, $(SOURCES))

all: build_only
	mv libasf_geocode.a $(LIBDIR)
	cp asf_geocode.h $(ASF_INCLUDE_DIR)
	rm -rf $(SHAREDIR)/projections
	cp -R projections/ $(SHAREDIR)
	cp WW15MGH.DAC $(SHAREDIR)

build_only: $(OBJS)
	rm -f libasf_geocode.a
	ar r libasf_geocode.a $(OBJS)
	$(RANLIB) libasf_geocode.a

$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*h)

clean:
	rm -rf $(OBJS) core.* core *~ libasf_geocode.a

parse_test: geocode_options.t.c
	$(CC) -DTEST_PARSE_OPTIONS $(INCLUDE_FLAGS) geocode_options.t.c \
	geocode_options.c parse_options.c $(LDLIBS) $(LDFLAGS) \
	-o geocode_options.t

# If any command responsible for producing a target exits with a
# nonzero exit status, delete that target.
.DELETE_ON_ERROR:
