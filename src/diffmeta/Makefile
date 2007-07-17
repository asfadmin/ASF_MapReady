PROGRAM := diffmeta

DEBUG_BUILD = 1

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

SOURCES := diffmeta.c

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
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)


LIBS  = \
	$(LIBDIR)/asf_meta.a \
	$(LIBDIR)/asf.a \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	-lm \
	-lz

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

all: diffmeta

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
	mv diffmeta$(BIN_POSTFIX) $(BINDIR)

.PHONY: clean
clean:
	rm -f $(PROGRAM) $(OBJS) core.* core
