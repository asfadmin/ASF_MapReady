# Makefile for: asf.a, our standard included library.
# Contains: 
# caplib, our error-protected standard library routines.
# fileUtil, a convenient set of routines for manipulating files.
# stopwatch, an easy-to-use set of timing routines
# cla, command line argument parsing
# log, routines to handle log files
# error, proper handling of error messages

include ../../make_support/system_rules

# Static libraries used by this library.  These get pulled out of the
# library repository and expanded, then recombined together with the
# objects from this library to save user having to link against all
# libraries used by this library and give some semblance of modularity
# to this benighted C language.
LIBS = asf_meta.a

OBJS  = caplib.o \
	fileUtil.o \
	stopwatch.o \
	cla.o \
	log.o \
	error.o

all: build_only
	mv asf.a $(LIBDIR)
	# Make a link obeying the usual naming convention.
	ln -s -f $(LIBDIR)/asf.a $(LIBDIR)/libasf.a
	rm *.o

build_only: $(OBJS) cook_libs
	rm -rf asf.a
	ar rv asf.a $(OBJS) $(wildcard ./library_cooking_pot/*.o)
	$(RANLIB) asf.a

cook_libs: $(addprefix $(LIBDIR)/, $(LIBS)) library_cooking_pot
	cd library_cooking_pot; \
	for lib in $(LIBS); do \
	  ar xv ../$(LIBDIR)/$$lib; \
	done

library_cooking_pot:
	mkdir library_cooking_pot

# Rebuild everything if makefile or any local of ASF header or library
# has changed.
$(OBJS): Makefile $(wildcard *.h) $(wildcard ../../include/*.h) \
	 $(addprefix $(LIBDIR)/, $(LIBS))

check: unit_tests.c build_only
	$(CC) $(CFLAGS) $< asf.a -lcheck -lm $(LDFLAGS) -o asf_tester
	./asf_tester

clean: 
	rm -rf $(OBJS) asf.a test_data/*.img asf_tester library_cooking_pot

distclean:
	rm -f core *~ TAGS gdb_init.com
