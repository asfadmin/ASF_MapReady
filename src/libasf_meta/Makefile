# libasf_meta: a library to fetch metadata from disparate sources.
# Includes facilities to geolocate, extract critical angles from,
# and do interferometric processing on SAR images.
#   Now incorporates asf_ceos library, as well.

include ../../make_support/system_rules

CFLAGS += -O0
# Static libraries used by this library.  These get pulled out of the
# library repository and expanded, then recombined together with the
# objects from this library to save user having to link against all
# libraries used by this library and give some semblance of modularity
# to this benighted C language.
LIBS = 	$(LIBDIR)/asf.a \
	$(LIBDIR)/libasf_reporting.a

LDLIBS = -lm

OBJS = 	asf_meta1x_to_meta09.o \
	asf_meta_init.o \
	asf_meta_read.o \
	asf_meta_read09.o \
	asf_meta_read1x.o \
	asf_meta_write.o \
	asf_meta_write09.o \
	asf_meta_write1x.o \
	las.o \
	earth_radius2datum.o \
	extract_meta_parameters.o \
	meta_fetch.o


all: libasf_meta test

libasf_meta: build_only
	mv libasf_meta.a $(LIBDIR)

build_only: $(OBJS)
	rm -rf libasf_meta.a	# Get rid of all stale symbols.
	ar r libasf_meta.a $(OBJS) $(wildcard ./library_cooking_pot/*.o)
	$(RANLIB) libasf_meta.a

# Rebuild everything if makefile or any local or ASF header or library
# or any lex or yacc file has changed.  The line commented out uses a
# target local variable to keep track of the local headers which are
# not automaticly generated, useful to prevent make from rebuilding
# everything if the lex and yacc objects aren't first in the object
# list.
#$(OBJS): LOCAL_HEADERS = $(subst y.tab.h, , $(wildcard *.h))
$(OBJS): Makefile $(LOCAL_HEADERS) $(wildcard ../../include/*.h) \
	$(LIBS) $(LDLIBS)

clean:
	rm -rf $(OBJS)

distclean:
	rm -f core *~ TAGS gdb_init.com

# Testing

test: build_only
	$(CC) $(CFLAGS) test_meta.c -o test_meta $(LIBDIR)/libasf_meta.a $(LIBS) $(LDFLAGS)
