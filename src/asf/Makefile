# Makefile for: asf.a, our standard included library.
# Contains:
# caplib, our error-protected standard library routines.
# fileUtil, a convenient set of routines for manipulating files.
# stopwatch, an easy-to-use set of timing routines
# cla, command line argument parsing
# log, routines to handle log files
# error, proper handling of error messages

include ../../make_support/system_rules

OBJS  = caplib.o \
	cla.o \
	endian.o \
	error.o \
	fileUtil.o \
	get_ceos_names.o \
	get_stf_names.o \
	ioLine.o \
	log.o \
	stopwatch.o \
	share.o \
	system.o \
	tmpdir.o

all: build_only
	mv asf.a $(LIBDIR)
	# Make a link obeying the usual naming convention.
	ln -s -f $(LIBDIR)/asf.a $(LIBDIR)/libasf.a
	rm *.o

build_only: $(OBJS)
	rm -f asf.a
	ar r asf.a $(OBJS)
	$(RANLIB) asf.a

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
