PROGRAM := asf_geocode

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

SOURCES := asf_geocode.c

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

LIBS :=	\
	$(LIBDIR)/libasf_geocode.a \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_raster.a \
	$(LIBDIR)/asf.a \
	$(GSL_LIBS) \
	$(JPEG_LIBS) \
	$(PROJ_LIBS) \
	$(GLIB_LIBS) \
	-lm

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

# Object files.
OBJS := $(patsubst %.c, %.o, $(SOURCES))

$(PROGRAM): $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS) $(LDLIBS) -o $@
	mv $(PROGRAM)$(BIN_POSTFIX) $(BINDIR)

.PHONY: clean
clean:
	rm -rf $(PROGRAM) $(OBJS) core.* core *~ $(PROGRAM)

# If any command responsible for producing a target exits with a
# nonzero exit status, delete that target.
.DELETE_ON_ERROR:
