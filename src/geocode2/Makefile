PROGRAM := geocode2

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

SOURCES := geocode_options.c geocode2.c

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

INCLUDE_FLAGS := -I$(ASF_INCLUDE_DIR) -I$(HOME)/local/include \
                  $(shell pkg-config --cflags-only-I glib-2.0)

CPPFLAGS := -D_GNU_SOURCE -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64 \
            $(INCLUDE_FLAGS)

# I'd like to use -Wconversion as advised by the GSL, but it seems to
# make it impossible to pass arguments of type float to functions that
# take type float (still get a complaint about argument being passed
# as float due to prototype rather than double.
#          -Wconversion -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align
CFLAGS := -Werror -Wall -W -Wmissing-prototypes -Wstrict-prototypes \
          -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align \
          -Wwrite-strings -Wnested-externs -fno-common -g -O4 \
          $(shell pkg-config --cflags-only-other glib-2.0)

LIBS :=	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf.a \
	$(LIBDIR)/libgsl.a \
	$(LIBDIR)/libgslcblas.a \
	$(LIBDIR)/libproj.a \
	-lm

LDLIBS := $(shell pkg-config --libs glib-2.0) $(LIBS)

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

$(PROGRAM): $(OBJS)
	$(CC) $(OBJS) $(LDLIBS) -o $@

.PHONY: clean
clean:
	rm -rf $(PROGRAM) $(OBJS) core.* core *~ $(PROGRAM)

# If any command responsible for producing a target exits with a
# nonzero exit status, delete that target.
.DELETE_ON_ERROR:
